package models

import play.api.db.slick.Config.driver.simple._

// models depenencies
import models.playSlickHelpers._
import models.playSlickHelpers.implicits._
import models.playSlickHelpers.implicits.more._
import models.entities._
import models.tables._

package object autojoins{
  // auto-join conditions
  implicit def autojoin1 = joinCondition[Computers,Devices]  (_.id === _.computerId)
  implicit def autojoin2 = joinCondition[Companies,Computers](_.id === _.companyId)
  implicit def autojoin3 = joinCondition[Sites,HasSite]      (_.id === _.siteId)

  implicit def autojoin4  = complexAutoJoin[Site,Computer,Sites,Computers]{
    case(sites,computers,joinType) =>
      import tableQueries._
      sites.autoJoin(Devices,joinType).further(computers,joinType)
  }
}