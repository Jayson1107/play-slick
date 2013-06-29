package models
package entities

import java.util.Date

import interfaces._
import types._

case class Company(
  name: String,
  typedId: Option[CompanyId] = None,
  dummy : Int = 0 // dummy column working around https://github.com/slick/slick/issues/40
) extends HasTypedId[CompanyId] with HasName

case class Computer(
  name: String,
  introduced: Option[Date] = None,
  discontinued: Option[Date] = None,
  companyId: Option[CompanyId] = None,
  typedId: Option[ComputerId] = None
) extends HasTypedId[ComputerId] with HasName

case class Device(
  computerId: ComputerId,
  siteId: SiteId,
  acqusition: Date,
  price: Double,
  typedId : Option[DeviceId] = None
) extends HasTypedId[DeviceId]

case class Site(
  name: String,
  typedId: Option[SiteId] = None,
  dummy : Int = 0 // dummy column working around https://github.com/slick/slick/issues/40
) extends HasTypedId[SiteId] with HasName

case class ResearchSite(
  siteId: SiteId,
  size: Size,
  typedId: Option[ResearchSiteId] = None
) extends HasTypedId[ResearchSiteId]

case class ProductionSite(
  siteId:SiteId,
  productionVolume:Int,
  typedId: Option[ProductionSiteId] = None
) extends HasTypedId[ProductionSiteId]

case class Site2(
  name: String,
  untypedId: Option[Long] = None
) extends HasUntypedId with HasName
