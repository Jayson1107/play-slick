package models
package entities

// models internal dependencies
import types._
import java.util.Date

// interfaces
trait Entity
trait HasId extends Entity{
  def id: Option[Long]
}
trait HasName extends Entity{
  def name: String
}

// entity classes
case class Company(
  id: Option[Long] = None,
  name: String
) extends HasId with HasName

case class Computer(
  id: Option[Long] = None,
  name: String,
  introduced: Option[Date] = None,
  discontinued: Option[Date] = None,
  companyId: Option[Long] = None
) extends HasId with HasName

case class Device(
  id: Option[Long] = None,
  computerId: Long,
  locationId: Long,
  acqusition: Date,
  price: Double
) extends HasId

case class Site(
  id: Option[Long] = None,
  name: String
) extends HasId with HasName

case class ResearchSite(
  id: Option[Long] = None,
  siteId: Long,
  size: Size
) extends HasId

case class ProductionSite(
  id: Option[Long] = None,
  siteId:Long,
  productionVolume:Int
) extends HasId
