package models
import play.api.db.slick.Config.driver.simple._

/**
 Tables already converted to queries, to reduce boilerplate an avoid confusion when conversions happens implicitly and when Query(_) must be applied explicitly.
 */
package object tables{
  def Companies       = Query(schema.Companies)
  def Computers       = Query(schema.Computers)
  def Devices         = Query(schema.Devices)
  def Sites           = Query(schema.Sites)
  def ResearchSites   = Query(schema.ResearchSites)
  def ProductionSites = Query(schema.ProductionSites)
}
