package models
import play.api.db.slick.Config.driver.simple._

/**
 Tables already converted to queries, to reduce boilerplate an avoid confusion when conversions happens implicitly and when Query(_) must be applied explicitly.
 */
package object tables{
  val Companies       = Query(schema.Companies)
  val Computers       = Query(schema.Computers)
  val Devices         = Query(schema.Devices)
  val Sites           = Query(schema.Sites)
  val ResearchSites   = Query(schema.ResearchSites)
  val ProductionSites = Query(schema.ProductionSites)
}
