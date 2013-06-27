package models
package entities

import java.util.Date

import interfaces._
import types._

case class Company(
  name: String,
  id: Option[Long] = None,
  dummy : Boolean = false // dummy column working around https://github.com/slick/slick/issues/40
) extends HasId /*[CompanyId]*/ with HasName

case class Computer(
  name: String,
  introduced: Option[Date] = None,
  discontinued: Option[Date] = None,
  companyId: Option[Long] = None,
  id: Option[Long] = None
) extends HasId /*[ComputerId]*/ with HasName

case class Device(
  computerId: Long,
  locationId: Long,
  acqusition: Date,
  price: Double,
  id : Option[Long] = None
) extends HasId /*[DeviceId]*/

case class Site(
  name: String,
  id: Option[Long] = None,
  dummy : Boolean = false // dummy column working around https://github.com/slick/slick/issues/40
) extends HasId /*[SiteId]*/ with HasName

case class Site2(
  name: String,
  id: Option[Long] = None
) extends HasId /*[SiteId]*/ with HasName

case class ResearchSite(
  siteId: Long,
  size: Size,
  id: Option[Long] = None
) extends HasId /*[ResearchSiteId]*/

case class ProductionSite(
  siteId:Long,
  productionVolume:Int,
  typedId: Option[ProductionSiteId] = None
) extends HasTypedId[ProductionSiteId]
