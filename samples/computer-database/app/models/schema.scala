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
package object schema{
  import java.util.Date // TODO: remove

  import play.api.db.slick.driver.simple._

  import util.tuples._
  import util.schema._
  import util.schema.NameGenerator._

  import entities._
  import schema.interfaces._
  import types._

  def allTables = play.api.db.slick.AutoDDL.tables("default")

  val tables = play.api.db.slick.AutoDDL.allTables
  import tables._

  def byName = allTables.map( t => t.entityNamePlural.toLowerCase -> (t/*:Any*/) ).toMap

  class Companies extends PowerTable[Company,CompanyId]("COMPANY") with HasName with HasDummy{
    val mapping = Mapping( Company.tupled )( Company.unapply )
    def columns = name ~ id.? ~ dummy

    def data = name ~ dummy
    def ?         = columns mapToOption
    def autoIncCount = data    mapInsert{ case data :+ id :+ dummy => data :+ dummy }
  }
  // For NULLable columns use Option[..] types (NOT O.Nullable as Slick infers that automatically)

  class Computers extends PowerTable[Computer,ComputerId]("COMPUTER") with HasName with HasDummy{
    val mapping = Mapping( Computer.tupled )( Computer.unapply )
    def columns = data ~ id.?

    def data         = name ~ introduced ~ discontinued ~ companyId
    def introduced   = column[Option[Date]]("introduced")
    def discontinued = column[Option[Date]]("discontinued")
    def companyId    = column[Option[CompanyId]]("company_id")

    def company       = foreignKey(fkName,companyId,Companies)(_.id)

    def ?       = columns mapToOption
    def autoIncCount = data    mapInsert{ case data :+ id => data }
  }

  class Sites extends PowerTable[Site,SiteId]("SITE") with HasName with HasDummy{// with AutoInc[Site]{
    val mapping = Mapping( Site.tupled )( Site.unapply )
    def columns = name ~ id.? ~ dummy

    def data = name ~ dummy
    
    def ?         = columns mapToOption
    def autoIncCount = data    mapInsert{ case data :+ id :+ dummy => data :+ dummy }
  }
  
  class Devices extends PowerTable[Device,DeviceId]("DEVICE") with HasSite{
    val mapping = Mapping( Device.tupled )( Device.unapply )
    def columns = data ~ id.?

    def data    = computerId ~ siteId ~ acquisition ~ price
    def computerId  = column[ComputerId]("computer_id")
    def acquisition = column[Date]("aquisition")
    def price       = column[Double]("price")

    def computer = foreignKey(fkName,computerId,Computers)(_.id)
    def idx = index(idxName, (computerId, siteId), unique=true)

    def ?       = columns mapToOption
    def autoIncCount = data    mapInsert{ case data :+ id => data }
  }
    /**
      * used for fetching whole Device object after outer join, also example autojoins-1-n
      * mapping all columns to Option using .? and using the mapping to the special
      * applyOption and unapplyOption constructor/extractor methods is s
      */
      /////////////
  class ResearchSites extends PowerTable[ResearchSite,ResearchSiteId]("RESEARCH_SITE") with HasExclusiveSite{
    val mapping = Mapping( ResearchSite.tupled )( ResearchSite.unapply )
    def columns = data ~ id.?

    def data = siteId ~ size
    def size = column[Size]("size",O.DBType("INT(1)"))
 
    def ?       = columns mapToOption
    def autoIncCount = data    mapInsert{ case data :+ id => data }
  }

  class ProductionSites extends PowerTable[ProductionSite,ProductionSiteId]("PRODUCTION_SITE") with HasExclusiveSite{
    val mapping = Mapping( ProductionSite.tupled )( ProductionSite.unapply )
    def columns = data ~ id.?

    def data = siteId ~ volume
    def volume = column[Int]("volume")
    
    def ?       = columns mapToOption
    def autoIncCount = data    mapInsert{ case data :+ id => data }
  }

  // TODO add example for table without id column

  // Slick currently does not allow mapping between single columns and objects,
  // this affects also inserting into only one column.
  // One workaround is using another dummy column like for Sites and Companies above,
  // but it requires changes to the database schema. Another option is simple not
  // mapping single column, but allow inserts as plain values.
  // Also see https://github.com/slick/slick/issues/40
  class Sites2 extends SingleColumnTable[Site2,SiteId]("SITE") with HasName{
    val mapping = Mapping( Site2.tupled )( Site2.unapply )

    def columns = name ~ untypedId.?
    
    def ?         = columns mapToOption
    def autoIncCount = name
    def autoIncId = name returning untypedId
  }
}
