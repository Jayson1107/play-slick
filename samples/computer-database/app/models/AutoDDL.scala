package play.api.db.slick
object AutoDDL extends AutoDDLInterface{
  object allTables{
	import models.schema
  	val Companies = new schema.Companies
  	val Computers = new schema.Computers
  	val Devices = new schema.Devices
  	val Sites = new schema.Sites
  	val ResearchSites = new schema.ResearchSites
  	val ProductionSites = new schema.ProductionSites
  }
  import allTables._
  def tables = Map(
  	"default" -> Seq( Companies,  Computers,  Devices,  Sites,  ResearchSites,  ProductionSites )
  )
}