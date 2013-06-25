package models

//models dependencies
import queries._
import entities._

case class Page[A](items: Seq[A], page: Int, size:Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

package object idConversions{
  import play.api.db.slick.Config.driver.simple._
  import tables._
  // enable using ids as queries
  implicit def idToComputerQuery[E <: Computer]( id:Long ) = Computers.byId(id)
  implicit def idToCompanyQuery[E <: Company]( id:Long )   = Companies.byId(id)
  // objects as ids (may not be a good idea :), up to you)
  //implicit def objectToId( o:entities.HasId ) = o.id.getOrElse(throw new Exception("id was None when trying to convert object to id"))
}
