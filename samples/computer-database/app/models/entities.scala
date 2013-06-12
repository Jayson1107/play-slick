package models.entities

// models internal dependencies
import models.types._
import java.util.Date

trait Entity
trait HasId extends Entity{
  def id: Option[Long]
}
trait HasName extends Entity{
  def name: String
}
case class Company (id: Option[Long] = None, name: String) extends HasId with HasName
case class Computer(id: Option[Long] = None, name: String, introduced: Option[Date]= None, discontinued: Option[Date]= None, companyId: Option[Long]=None) extends HasId with HasName
case class Device  (id: Option[Long] = None, computerId: Long, locationId: Long, acqusition: Date, price: Double ) extends HasId
object Device{
  /** for outer joins */
  def applyOption(id: Option[Long], computerId: Option[Long], locationId: Option[Long], acqusition: Option[Date], price: Option[Double] )
  = id.map{
    _ =>
    Device( id,computerId.get,locationId.get,acqusition.get,price.get )
  }
}
case class Site    (id: Option[Long] = None, name:String) extends HasId with HasName
case class ResearchSite  (id: Option[Long] = None, siteId:Long, size:Size) extends HasId
case class ProductionSite(id: Option[Long] = None, siteId:Long, productionVolume:Int) extends HasId
