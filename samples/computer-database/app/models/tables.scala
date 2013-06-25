/** Description of the Database schema
  * 
  * Each database table is described using a so called Table object which extends Table.
  * In fact it is better to think of them as prototypes for rows. To add functionality to
  * a row which can be used in a query like extra methods, add them to the Table object.
  *
  * Do not use table object directly to start a query, but wrap them in a Query(...) call instead like shown in tables.scala.
  * 
  * Query(Companies).filter(_.name === "Apple Inc.")
  * 
  * To define Table objects, please use the idiom
  * 
  * val Companies = new Companies
  * class company extends BaseTable ...
  * 
  * and NOT object Companies extends BaseTable ...
  * because object only works in simple cases but breaks in complex
  * cases because of https://issues.scala-lang.org/browse/SI-3764
  *
  *
  */
package models

import play.api.db.slick.Config.driver.simple._

import scala.reflect.runtime.universe.TypeTag

import java.util.Date // TODO: remove

// slick dependencies
import slick.lifted.{Projection,ColumnBase,MappedProjection}

// model internal dependencies
import models.entities._
import types._
import tuples._
import projections._

trait TableHelpers{
  // generate unique names for foreign keys and indices
  var fk_inc = 0
  var idx_inc = 0
  def fkName(name:String="")  = s"fk_${name}_" +{fk_inc += 1;fk_inc}
  def fkName : String = fkName()
  def idxName(name:String="") = s"idx_${name}_"+{idx_inc += 1;idx_inc}
  def idxName : String = idxName()
  
  // generic column helpers (easier as functions rather than extension methods)
  def iLike( lhs:Column[String], rhs:Column[String] ) = lhs.toLowerCase like rhs.toLowerCase

  def insertWithIdException = throw new Exception("Cannot insert object with id != None using autoInc, please remove id or insert into * instead.")
}

/*
  implicit def mappingHelpers  [T <: Product]( p:Projection[T] ) = new{
    def mapInsert[R]( from: R => T ) = mapWith[T,R] (p) (_ => ???) (from)
    def mapOption[R]( to:   T => R ) = mapWith[T,R] (p) (to)   (_ => ???)
  }
  def mapWith[T <: Product,R]( p:Projection[T] )( to: T => R )( from: R => T ) = p <> (to,(x:R) => Some(from(x)))
*/

package object schema{
  def allTables = {
    Seq( Companies, Computers, Devices, ResearchSites, ProductionSites )
  }
  def tableByName = allTables.map( t => t.entityNamePlural.toLowerCase -> (t:Any) ).toMap

  object interfaces{
    trait HasId{
      this:Table[_]=>
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    }
    trait HasName extends TableHelpers{
      this:Table[_] =>
      def name = column[String]("name", O.NotNull)
      def byName( pattern:Column[String] ) = iLike( name, pattern )
    }
    trait HasSite extends TableHelpers{
      this:Table[_] =>
      def siteId = column[Long]("site_id")
      def site  = foreignKey(fkName,siteId,Sites)(_.id)
    }
    trait HasExclusiveSite extends HasSite{
      this:Table[_]=>
       def idx = index(idxName, (siteId), unique = true)
    }
    trait MappingHelpers[E]{
      val mapping : Mapping[E]
      import mapping._
      implicit def mappingHelpers2  [T <: Product]( p:Projection[T] ) = new{
        def mapInsert( from: Columns => T ) = mapWith[E](_ => ???, (e:E) => extract(e).map(from))
        def mapOption( to:   T => Option[E] ) = mapWith[Option[E]] (to, _ => ???)
        def mapWith[E]( to: T => E, from: E => Option[T] ) = p <> (to,from)
      }
    }
    trait StarProjection[E] extends MappingHelpers[E]{
      import mapping._
      def columns : Projection[Columns]
      def * = columns mapWith (create,extract)
    }
    trait OptionMapping[E] extends Table[E]{
      def ? : ColumnBase[Option[E]]      
    }
    trait AutoInc[E] extends Table[E] with HasId{
      def autoInc : ColumnBase[E]
      def autoIncId = autoInc returning id
    }
    trait Mapping[E]{
      type Columns <: Product
      def create : Columns => E
      def extract   : E => Option[Columns]
    }
    def Mapping[E,C <:Product]( to: C => E )( from: E => Option[C] ) = new Mapping[E]{
      type Columns = C
      def create = to
      def extract = from
    }
    abstract class MyTable[E:TypeTag]( table: String ) extends Table[E](table:String){
      // FYI: database name can be accessed through inherited val tableName
      def entityNamePlural = this.getClass.getName.split("\\$").reverse.head
      def entityName       = implicitly[TypeTag[E]].tpe.typeSymbol.name.decoded
    }
    trait SemiFeatured[E]  extends ProjectionsOptionLifting[E] with HasId with StarProjection[E] with OptionMapping[E]
    trait FullyFeatured[E] extends SemiFeatured[E] with AutoInc[E]
    abstract class PowerTable[E:TypeTag]( table: String ) extends MyTable(table) with FullyFeatured[E]
    abstract class SingleColumnTable[E:TypeTag]( table: String ) extends MyTable(table) with SemiFeatured[E]
  }
  import interfaces._

  val Companies = new Companies
  class Companies extends SingleColumnTable[Company]("COMPANY") with HasName{
    val mapping = Mapping( Company.tupled )( Company.unapply )

    def columns = name ~ id.?
    
    def ?         = columns mapToOption
    def autoInc   = name
    def autoIncId = name returning id
  }
  // For NULLable columns use Option[..] types (NOT O.Nullable as Slick infers that automatically)

  val Computers = new Computers
  class Computers extends PowerTable[Computer]("COMPUTER") with HasName{
    type Columns = (String,Option[Date],Option[Date],Option[Long])
    val mapping = Mapping( Computer.tupled )( Computer.unapply )
    def columns = data ~ id.?

    def data         = name ~ introduced ~ discontinued ~ companyId
    def introduced   = column[Option[Date]]("introduced")
    def discontinued = column[Option[Date]]("discontinued")
    def companyId    = column[Option[Long]]("company_id")

    def company       = foreignKey(fkName,companyId,Companies)(_.id)

    def ?       = columns mapToOption
    def autoInc = data    mapInsert{ case data :+ id => data }  }

  val Sites = new Sites
  class Sites extends SingleColumnTable[Site]("SITE") with HasName{// with AutoInc[Site]{
    val mapping = Mapping( Site.tupled )( Site.unapply )

    def columns = name ~ id.?
    
    def ?         = columns mapToOption
    def autoInc   = name
    def autoIncId = name returning id
}
  
  val Devices = new Devices
  class Devices extends PowerTable[Device]("DEVICE") with HasSite{
    val mapping = Mapping( Device.tupled )( Device.unapply )
    def columns = data ~ id.?

    def data    = computerId ~ siteId ~ acquisition ~ price
    def computerId  = column[Long]("computer_id")
    def acquisition = column[Date]("aquisition")
    def price       = column[Double]("price")

    def computer = foreignKey(fkName,computerId,Computers)(_.id)
    def idx = index(idxName, (computerId, siteId), unique=true)

    def ?       = columns mapToOption
    def autoInc = data    mapInsert{ case data :+ id => data }
  }
    /**
      * used for fetching whole Device object after outer join, also example autojoins-1-n
      * mapping all columns to Option using .? and using the mapping to the special
      * applyOption and unapplyOption constructor/extractor methods is s
      */
      /////////////
  val ResearchSites = new ResearchSites
  class ResearchSites extends PowerTable[ResearchSite]("RESEARCH_SITE") with HasExclusiveSite{
    val mapping = Mapping( ResearchSite.tupled )( ResearchSite.unapply )
    def columns = data ~ id.?

    def data = siteId ~ size
    def size = column[Size]("size",O.DBType("INT(1)"))
 
    def ?       = columns mapToOption
    def autoInc = data    mapInsert{ case data :+ id => data }
  }

  val ProductionSites = new ProductionSites
  class ProductionSites extends PowerTable[ProductionSite]("PRODUCTION_SITE") with HasExclusiveSite{
    val mapping = Mapping( ProductionSite.tupled )( ProductionSite.unapply )
    def columns = data ~ id.?

    def data = siteId ~ volume
    def volume = column[Int]("volume")
    
    def ?       = columns mapToOption
    def autoInc = data    mapInsert{ case data :+ id => data }
  }
}
