package models
/**
 Tables already converted to queries, to reduce boilerplate an avoid confusion when conversions happens implicitly and when Query(_) must be applied explicitly.
 */
package object tables{
  import play.api.db.slick.Config.driver.simple._

  def Companies       = Query(schema.tables.Companies)
  def Computers       = Query(schema.tables.Computers)
  def Devices         = Query(schema.tables.Devices)
  def Sites           = Query(schema.tables.Sites)
  def ResearchSites   = Query(schema.tables.ResearchSites)
  def ProductionSites = Query(schema.tables.ProductionSites)
}
