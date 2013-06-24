package models
package entities

// models internal dependencies
import types._
import java.util.Date

package interfaces{
  // interfaces
  trait Entity
  trait HasId extends Entity{
    def id: Option[Long]
  }
  trait HasName extends Entity{
    def name: String
  }
}
import interfaces._
// entity classes
case class Company(
  name: String,
  id: Option[Long] = None
) extends HasId with HasName

case class Computer(
  name: String,
  introduced: Option[Date] = None,
  discontinued: Option[Date] = None,
  companyId: Option[Long] = None,
  id: Option[Long] = None
) extends HasId with HasName

case class Device(
  computerId: Long,
  locationId: Long,
  acqusition: Date,
  price: Double,
  id : Option[Long] = None
) extends HasId

case class Site(
  name: String,
  id: Option[Long] = None
) extends HasId with HasName

case class ResearchSite(
  siteId: Long,
  size: Size,
  id: Option[Long] = None
) extends HasId

case class ProductionSite(
  siteId:Long,
  productionVolume:Int,
  id: Option[Long] = None
) extends HasId
