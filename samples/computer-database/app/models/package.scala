package models

// slick dependencies
import slick.ast.{JoinType}//TableNode,
import slick.lifted.{MappedTypeMapper}//{ColumnOption}//,TypeMapper,ForeignKeyAction,ForeignKeyQuery,,Index,Join, MappedTypeMapper, MappedProjection, Projection,Shape}

//models dependencies
import queryExtensions._
import entities._
/*
package object tables{
  def allTables = {
    Seq( Companies, Computers, Devices, Sites, ResearchSites, ProductionSites )
  }
  def tableByName = allTables.map( t => t.tableNamePlural.toLowerCase -> t ).toMap	
  val Companies = new Companies
  val Computers = new Computers
  val Devices = new Devices
  val Sites = new Sites
  val ProductionSites = new ProductionSites
  val ResearchSites = new ResearchSites
}*/
/*
package object playSlickHelpers{
  import play.api.db.slick.Config.driver.simple._
  def autoJoin[L,R]( condition : (L,R) => Column[Boolean] ) = new ReversableJoinCondition(condition)
  def complexAutoJoin[LE,RE,LT,RT]( autoJoin : (Query[LT,LE],Query[RT,RE],JoinType) => Query[(LT,RT),(LE,RE)] ) = new AutoJoin(autoJoin)
}*/
/*
package object types{
  // TODO: get rid of this mapping (which was probably only there to support play's .format method extension)
  implicit val javaUtilDateTypeMapper = MappedTypeMapper.base[java.util.Date, java.sql.Date](
    x => new java.sql.Date(x.getTime),
    x => new java.util.Date(x.getTime)
  )	
}
*/
case class Page[A](items: Seq[A], page: Int, size:Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

package object idConversions{
  import play.api.db.slick.Config.driver.simple._
  import tableQueries._
  // enable using ids as queries
  implicit def idToComputerQuery[E <: Computer]( id:Long ) = Computers.byId(id)
  implicit def idToCompanyQuery[E <: Company]( id:Long )   = Companies.byId(id)
  // objects as ids (may not be a good idea :), up to you)
  //implicit def objectToId( o:entities.HasId ) = o.id.getOrElse(throw new Exception("id was None when trying to convert object to id"))
}
