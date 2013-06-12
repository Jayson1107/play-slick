package models
import play.api.db.slick.Config.driver.simple._

/**
 Tables already converted to queries, to reduce boilerplate an avoid confusion when conversions happens implicitly and when Query(_) must be applied explicitly.
 */
package object tableQueries{ 
  val Companies       = Query(tables.Companies)
  val Computers       = Query(tables.Computers)
  val Devices         = Query(tables.Devices)
  val Sites           = Query(tables.Sites)
  val ResearchSites   = Query(tables.ResearchSites)
  val ProductionSites = Query(tables.ProductionSites)
}
