package models.schema

import scala.reflect.runtime.universe.TypeTag

import play.api.db.slick.Config.driver.simple._

import util.projections._
import util.queries._
import util.schema._

import models.types._
import scala.slick.lifted.BaseTypeMapper

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
  }
  trait Features[E] extends ProjectionsOptionLifting[E] with HasId with HasTypedId with StarProjection[E] with OptionMapping[E]
  abstract class SingleColumnTable[E:TypeTag,ID<:TypedId:BaseTypeMapper]( table: String ) extends MyTable(table) with Features[E]{
    type IdType = ID
    def typedId = column[IdType]("id", O.PrimaryKey, O.AutoInc)
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    import scala.reflect.runtime.universe.typeOf
    def entityNamePlural = typeOf[this.type].typeSymbol.name.decoded
    def entityName       = typeOf[E]        .typeSymbol.name.decoded
  }
  abstract class PowerTable       [E:TypeTag,ID<:TypedId:BaseTypeMapper]( table: String ) extends SingleColumnTable[E,ID](table) with AutoInc[E]
}