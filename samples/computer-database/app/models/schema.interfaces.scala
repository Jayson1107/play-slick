package models.schema
import scala.reflect.runtime.universe.TypeTag

import play.api.db.slick.Config.driver.simple._

import util.projections._

// slick dependencies
import slick.lifted.{Projection,ColumnBase,MappedProjection}

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

package object interfaces{
  def allTables = {
    Seq( Companies, Computers, Devices, ResearchSites, ProductionSites )
  }
  def tableByName = allTables.map( t => t.entityNamePlural.toLowerCase -> (t:Any) ).toMap

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