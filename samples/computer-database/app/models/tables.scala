package models

import play.api.db.slick.Config.driver.simple._

import scala.reflect.runtime.universe.TypeTag

import java.util.Date // TODO: remove
import models.types._

// slick dependencies
import slick.lifted.{Projection}

// model internal dependencies
import models.entities._
import models.playSlickHelpers._

object packageHelpers{
  // generate unique names for foreign keys and indices
  var fk_inc = 0
  var idx_inc = 0
  def fkName(name:String="")  = s"fk_${name}_" +{fk_inc += 1;fk_inc}
  def fkName : String = fkName()
  def idxName(name:String="") = s"idx_${name}_"+{idx_inc += 1;idx_inc}
  def idxName : String = idxName()
  
  // generic column helpers (easier as functions rather than extension methods)
  def iLike( lhs:Column[String], rhs:Column[String] ) = lhs.toLowerCase like rhs.toLowerCase
}
import packageHelpers._

abstract class BaseTable[E](table:String)(implicit etype:TypeTag[E]) extends Table[E](table:String) with HasId{
  def tableNamePlural   = this.getClass.getName.split("\\$").reverse.head
  def tableNameSingular = etype.tpe.typeSymbol.name.decoded
  def tableNameDb       = table.toLowerCase
  implicit val javaUtilDateTypeMapper = MappedTypeMapper.base[java.util.Date, java.sql.Date](
    x => new java.sql.Date(x.getTime),
    x => new java.util.Date(x.getTime)
  )
  def autoInc = * returning id
  def columns : Projection[_]
  def column(n:Int) = columns.productElement(n).asInstanceOf[Column[Any]]
}
trait HasId{
  this:Table[_]=>
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
}
trait HasName{
  this:Table[_]=>
  def name = column[String]("name", O.NotNull)
  def byName( pattern:Column[String] ) = iLike( name, pattern )
}

object tables{
  def allTables = {
    Seq( Companies, Computers, Devices, Sites, ResearchSites, ProductionSites )
  }
  def tableByName = allTables.map( t => t.tableNamePlural.toLowerCase -> t ).toMap

  trait HasSite{
    this:Table[_]=>
    def siteId = column[Long]("site_id")
    def site  = foreignKey(fkName,siteId,Sites)(_.id)
  }
  trait HasExclusiveSite extends HasSite{
   this:Table[_]=>
   def idx = index(idxName, (siteId), unique = true)
  }

  // table objects
  val Companies = new Companies
  class Companies extends BaseTable[Company]("COMPANY") with HasName{
    def columns = id.? ~ name
    def * = columns <>(Company.apply _, Company.unapply _)
  }
  val Computers = new Computers
  class Computers extends BaseTable[Computer]("COMPUTER") with HasName{
    def introduced    = column[Date]("introduced", O.Nullable)
    def discontinued  = column[Date]("discontinued", O.Nullable)
    def companyId     = column[Long]("company_id", O.Nullable)
    def company       = foreignKey(fkName,companyId,Companies)(_.id)
    //def companyOption = Query(this).filter(_.id === id).leftJoin(Companies).on(_.companyId === _.id).map(_._2)
    def columns = id.? ~ name ~ introduced.? ~ discontinued.? ~ companyId.?
    def * = columns <> (Computer.apply _, Computer.unapply _)

    // relationships
    // TODO fixme
    //def items : Query[Devices,Device] = extendBaseTableBlind(this).get(queries.Devices) // for( i <- Devices; c <- i.computer; if c.id === this.id ) yield i
    //def sites = items.flatMap( _.site )
  }
  val Sites = new Sites
  class Sites extends BaseTable[Site]("SITE") with HasName{
    def columns = id.? ~ name
    def * = columns <> (Site.apply _, Site.unapply _)
    // relationships
    def items : Query[Devices,Device] = for( i <- Devices; s <- i.site; if s.id === id ) yield i // Query(Devices).filter(_.siteId === id)//
    def computers = items.flatMap( _.computer )
  }
  def mapToOption[T <: Product,R]( p:Projection[T] )( to: T => R ) = mapOption(p)(to)((_:R) => None)
  def mapOption  [T <: Product,R]( p:Projection[T] )( to: T => R )( from: R => Option[T] ) = p <> (to,from)
  val Devices = new Devices
  class Devices extends BaseTable[Device]("DEVICE") with HasSite{// with Joinable[(Option[Long], Option[Long], Option[Long], Option[java.util.Date], Option[Double])]{
    def columns = computerId ~ siteId ~ acquisition ~ price
    def computerId = column[Long]("computer_id")
    def acquisition = column[Date]("aquisition")
    def price = column[Double]("price")
    def computer = foreignKey(fkName,computerId,Computers)(_.id)
    def * = id.? ~: columns <> (Device.apply _, Device.unapply _)
    def idx = index(idxName, (computerId, siteId), unique=true)
    /**
      * used for fetching whole Device object after outer join, also example autojoins-1-n
      * mapping all columns to Option using .? and using the mapping to the special
      * applyOption and unapplyOption constructor/extractor methods is s
      */
    def option[T] = mapToOption( id.? ~ computerId.? ~ siteId.? ~ acquisition.? ~ price.? ){
                      //case (id:Some[_],Some(_2),Some(_3),Some(_4),Some(_5)) => Some(Device(id,_2,_3,_4,_5))
                      case (_1:Some[_],_2,_3,_4,_5) => Some(Device(_1,_2.get,_3.get,_4.get,_5.get))
                      case _ => None
                    }
  }
  val ResearchSites = new ResearchSites
  class ResearchSites extends BaseTable[ResearchSite]("RESEARCH_SITE") with HasExclusiveSite{
    def size = column[Size]("size",O.DBType("INT(1)"))
    def columns = id.? ~ siteId ~ size
    def * = columns <> (ResearchSite.apply _, ResearchSite.unapply _)
  }
  val ProductionSites = new ProductionSites
  class ProductionSites extends BaseTable[ProductionSite]("PRODUCTION_SITE") with HasExclusiveSite{
    def volume = column[Int]("volume")
    def columns = id.? ~ siteId ~ volume
    def * = columns <> (ProductionSite.apply _, ProductionSite.unapply _)
  }
}

/*package models.tables

object packageHelpers{
  var fk_inc = 0
  var idx_inc = 0
  def fkName(name:String="")  = s"fk_${name}_" +{fk_inc += 1;fk_inc}
  def fkName : String = fkName()
  def idxName(name:String="") = s"idx_${name}_"+{idx_inc += 1;idx_inc}
  def idxName : String = idxName()
  
  // TODO: get rid of this mapping (which was probably only there to support play's .format method extension)
  implicit val javaUtilDateTypeMapper = MappedTypeMapper.base[java.util.Date, java.sql.Date](
    x => new java.sql.Date(x.getTime),
    x => new java.util.Date(x.getTime)
  ) 

  // generic column helpers (easier as functions rather than extension methods)
  def iLike( lhs:Column[String], rhs:Column[String] ) = lhs.toLowerCase like rhs.toLowerCase
}
import packageHelpers._
abstract class BaseTable[E](table:String)(implicit etype:TypeTag[E]) extends SlickBaseTable[E](table:String) with HasId{
  def tableNamePlural   = this.getClass.getName.split("\\$").reverse.head
  def tableNameSingular = etype.tpe.typeSymbol.name.decoded
  def tableNameDb       = table.toLowerCase
  def autoInc = * returning id
  def columns : Projection[_]
  def column(n:Int) = columns.productElement(n).asInstanceOf[Column[Any]]
}
trait HasId extends TableTrait{
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
}
trait HasName extends TableTrait{
  def name = column[String]("name", O.NotNull)
  def byName( pattern:Column[String] ) = iLike( name, pattern )
}

trait HasSite extends TableTrait{
  def siteId = column[Long]("site_id")
  def site  = foreignKey(fkName,siteId,Sites)(_.id)
}
trait HasExclusiveSite extends HasSite{
  def idx = index(idxName, (siteId), unique = true)
}

// table objects
class Companies extends BaseTable[Company]("COMPANY") with HasName{
  def columns = id.? ~ name
  def * = columns <>(Company.apply _, Company.unapply _)
}
class Computers extends BaseTable[Computer]("COMPUTER") with HasName{
  def introduced    = column[Date]("introduced", O.Nullable)
  def discontinued  = column[Date]("discontinued", O.Nullable)
  def companyId     = column[Long]("company_id", O.Nullable)
  def company       = foreignKey(fkName,companyId,Companies)(_.id)
  //def companyOption = Query(this).filter(_.id === id).leftJoin(Companies).on(_.companyId === _.id).map(_._2)
  def columns = id.? ~ name ~ introduced.? ~ discontinued.? ~ companyId.?
  def * = columns <> (Computer.apply _, Computer.unapply _)

  // relationships
  // TODO fixme
  //def items : Query[Devices,Device] = extendBaseTableBlind(this).get(queries.Devices) // for( i <- Devices; c <- i.computer; if c.id === this.id ) yield i
  //def sites = items.flatMap( _.site )
}
class Sites extends BaseTable[Site]("SITE") with HasName{
  def columns = id.? ~ name
  def * = columns <> (Site.apply _, Site.unapply _)
  // relationships
  def items : Query[Devices,Device] = for( i <- Devices; s <- i.site; if s.id === id ) yield i // Query(Devices).filter(_.siteId === id)//
  def computers = items.flatMap( _.computer )
}
class Devices extends BaseTable[Device]("ITEMS") with HasSite{
  def columns = id.? ~ computerId ~ siteId ~ acquisition ~ price
  def computerId = column[Long]("computer_id")
  def acquisition = column[Date]("aquisition")
  def price = column[Double]("price")
  def computer = foreignKey(fkName,computerId,Computers)(_.id)
  def * = columns <> (Device.apply _, Device.unapply _)
  def idx = index(idxName, (computerId, siteId), unique=true)
  def *? = id.? ~ computerId.? ~ siteId.? ~ acquisition.? ~ price.?
}
class ResearchSites extends BaseTable[ResearchSite]("RESEARCH_SITE") with HasExclusiveSite{
  def size = column[Size]("size",O.DBType("INT(1)"))
  def columns = id.? ~ siteId ~ size
  def * = columns <> (ResearchSite.apply _, ResearchSite.unapply _)
}
class ProductionSites extends BaseTable[ProductionSite]("PRODUCTION_SITE") with HasExclusiveSite{
  def volume = column[Int]("volume")
  def columns = id.? ~ siteId ~ volume
  def * = columns <> (ProductionSite.apply _, ProductionSite.unapply _)
}
*/