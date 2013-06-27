package models
package object relationships{
  import play.api.db.slick.Config.driver.simple._

  import util.autojoin._

  import models.schema.interfaces._
  import models.tables._

  // auto-join conditions
  implicit def autojoin1 = joinCondition(Computers,Devices)  (_.id === _.computerId)
  implicit def autojoin2 = joinCondition(Companies,Computers)(_.id === _.companyId)
  implicit def autojoin3 = joinCondition(Sites,InterfaceJoin[HasSite])(_.typedId === _.siteId)

  implicit def autojoin4 = complexJoin(Sites,Computers){
    joinType => _.autoJoin(Devices,joinType).further(_,joinType)
  }
}