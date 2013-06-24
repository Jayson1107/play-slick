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
import slick.lifted.{Projection,ColumnBase}

// model internal dependencies
import models.entities._
import types._
import tuples._

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
    Seq( Companies, Computers, Devices, Sites, ResearchSites, ProductionSites )
  }
  def tableByName = allTables.map( t => t.tableNamePlural.toLowerCase -> t ).toMap

  object interfaces{
    trait AutoInc[E] extends HasId{
      this:BaseTable[E]=>
      def autoInc2 : ColumnBase[E]
      def autoIncId = autoInc2 returning id
    }
    trait HasId{
      this:BaseTable[_]=>
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    }
    trait HasName{
      this:BaseTable[_]=>
      def name = column[String]("name", O.NotNull)
      def byName( pattern:Column[String] ) = iLike( name, pattern )
    }
    trait HasSite{
      this:BaseTable[_]=>
      def siteId = column[Long]("site_id")
      def site  = foreignKey(fkName,siteId,Sites)(_.id)
    }
    abstract class BaseTable[E](table:String)(implicit etype:TypeTag[E]) extends Table[E](table:String) with HasId with TableHelpers  {
      this:TableHelpers => 
      type Entity = E
      def tableNamePlural   = this.getClass.getName.split("\\$").reverse.head
      def tableNameSingular = etype.tpe.typeSymbol.name.decoded
      def tableNameDb       = table.toLowerCase
      implicit val javaUtilDateTypeMapper = MappedTypeMapper.base[java.util.Date, java.sql.Date](
        x => new java.sql.Date(x.getTime),
        x => new java.util.Date(x.getTime)
      )
      //type ColumnTypes
      //def autoInc = columns.shaped returning id
      def autoInc = * returning id
      def columns : ColumnBase[_]
      //def columns : ColumnBase[ColumnTypes]
    //  def column(n:Int) = columns.productElement(n).asInstanceOf[Column[Any]]
      implicit def mappingHelpers  [T <: Product]( p:Projection[T] ) = new{
        def mapInsert( from: E => Option[T] ) = mapWith[E]         (_ => ???, from)
        def mapOption( to:   T => Option[E] ) = mapWith[Option[E]] (to, _ => ???)
        def mapWith[E]( to: T => E, from: E => Option[T] ) = p <> (to,from)
      }
    }
  }
  import interfaces._
  trait HasExclusiveSite extends HasSite{
   this:BaseTable[_]=>
   def idx = index(idxName, (siteId), unique = true)
  }

  val Companies = new Companies
  class Companies extends BaseTable[Company]("COMPANY") with HasName{
    type ColumnTypes = String
    def columns = name
    def * = columns ~ id.? mapWith (create,extract)
    val extract = Company.unapply _
    val create  = (Company.apply _).tupled
  }
  val Computers = new Computers
  class Computers extends BaseTable[Computer]("COMPUTER") with HasName{
    type ColumnTypes = (String,Option[Date],Option[Date],Option[Long])
    // For NULLable columns use Option[..] types (NOT O.Nullable as Slick infers that automatically)
    def introduced    = column[Option[Date]]("introduced")
    def discontinued  = column[Option[Date]]("discontinued")
    def companyId     = column[Option[Long]]("company_id")
    def company       = foreignKey(fkName,companyId,Companies)(_.id)
    def columns = name ~ introduced ~ discontinued ~ companyId
    def * = columns ~ id.? mapWith (create,extract)
    val extract = Computer.unapply _
    val create  = (Computer.apply _).tupled
  }
  val Sites = new Sites
  class Sites extends BaseTable[Site]("SITE") with HasName{
    type ColumnTypes = (String)
    def columns = name
    def * = columns ~ id.? mapWith (create,extract)
    val extract = Site.unapply _
    val create  = (Site.apply _).tupled
  }
  
  val Devices = new Devices
  class Devices extends BaseTable[Device]("DEVICE") with HasSite with AutoInc[Device]{
    // joined columns
    type ColumnTypes    = ( Long, Long, Date, Double )
    type ColumnsOptions = ( Option[Long], Option[Long], Option[Long], Option[java.util.Date], Option[Double] )
    def columns       = computerId   ~ siteId   ~ acquisition   ~ price
    def optionColumns = computerId.? ~ siteId.? ~ acquisition.? ~ price.?

    // individual columns
    def computerId  = column[Long]("computer_id")
    def acquisition = column[Date]("aquisition")
    def price = column[Double]("price")

    // foreign keys
    def computer = foreignKey(fkName,computerId,Computers)(_.id)

    // indices
    def idx = index(idxName, (computerId, siteId), unique=true)

    val extract = Device.unapply _
    val create  = (Device.apply _).tupled

    // generic helpers (can be copy and pasted between table objects)
    def * = columns ~ id.? mapWith (create,extract)

    /**
      * used for fetching whole Device object after outer join, also example autojoins-1-n
      * mapping all columns to Option using .? and using the mapping to the special
      * applyOption and unapplyOption constructor/extractor methods is s
      */
    def ? = optionColumns ~ id.?.? mapOption { _.liftOption.map(create) }

    def autoInc2 = columns mapInsert{ extract(_).map{
      case data :+ None => data
      case _ => insertWithIdException
    }}

//    type Columns        = (Long, Long, Long, java.util.Date, Double)
  }
  val ResearchSites = new ResearchSites
  class ResearchSites extends BaseTable[ResearchSite]("RESEARCH_SITE") with HasExclusiveSite{
    type ColumnTypes = (Long,Size)
    def size = column[Size]("size",O.DBType("INT(1)"))
    def columns = siteId ~ size
    def * = columns ~ id.? <> (ResearchSite.apply _, ResearchSite.unapply _)
  }
  val ProductionSites = new ProductionSites
  class ProductionSites extends BaseTable[ProductionSite]("PRODUCTION_SITE") with HasExclusiveSite{
    type ColumnTypes = (Long,Int)
    def volume = column[Int]("volume")
    def columns = siteId ~ volume
    def * = columns ~ id.? <> (ProductionSite.apply _, ProductionSite.unapply _)
  }
}
