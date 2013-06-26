package models.schema

import scala.reflect.runtime.universe.TypeTag

import play.api.db.slick.Config.driver.simple._

import util.projections._
import util.queries._
import util.schema._


package object interfaces{
  /** This interface provides a dummy column to work around https://github.com/slick/slick/issues/40
    */
  trait HasDummy{
    this:Table[_]=>
    def dummy = column[Boolean]("dummy")
  }
  trait HasName extends NameGenerator{
    this:Table[_] =>
    def name = column[String]("name", O.NotNull)
    def byName( pattern:Column[String] ) = iLike( name, pattern )
  }
  trait HasSite extends NameGenerator{
    this:Table[_] =>
    def siteId = column[Long]("site_id")
    def site  = foreignKey(fkName,siteId,Sites)(_.id)
  }
  trait HasExclusiveSite extends HasSite{
    this:Table[_]=>
     def idx = index(idxName, (siteId), unique = true)
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