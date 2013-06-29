package models
package entities

import java.util.Date

import interfaces._
import types._

case class Company(
  name: String,
  id: Option[CompanyId] = None,
  dummy : Int = 0 // dummy column working around https://github.com/slick/slick/issues/40
) extends HasId[CompanyId] with HasName

case class Computer(
  name: String,
  introduced: Option[Date] = None,
  discontinued: Option[Date] = None,
  companyId: Option[CompanyId] = None,
  id: Option[ComputerId] = None
) extends HasId[ComputerId] with HasName

case class Device(
  computerId: ComputerId,
  siteId: SiteId,
  acqusition: Date,
  price: Double,
  id : Option[DeviceId] = None
) extends HasId[DeviceId]

case class Site(
  name: String,
  id: Option[SiteId] = None,
  dummy : Int = 0 // dummy column working around https://github.com/slick/slick/issues/40
) extends HasId[SiteId] with HasName

case class ResearchSite(
  siteId: SiteId,
  size: Size,
  id: Option[ResearchSiteId] = None
) extends HasId[ResearchSiteId]

case class ProductionSite(
  siteId:SiteId,
  productionVolume:Int,
  id: Option[ProductionSiteId] = None
) extends HasId[ProductionSiteId]

case class Site2(
  name: String,
  untypedId: Option[Long] = None
) extends HasUntypedId with HasName
