package models

case class Page[A](items: Seq[A], page: Int, size:Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

package object idConversions{
  import play.api.db.slick.driver.simple._

  import entities._
  import queries._
  import tables._

  // enable using ids as queries
  implicit def idToComputerQuery[E <: Computer]( id:Long ) = Computers.byUntypedId(id)
  implicit def idToCompanyQuery [E <: Company] ( id:Long ) = Companies.byUntypedId(id)
  // objects as ids (may not be a good idea :))
  //implicit def objectToId( o:entities.HasId ) = o.id.getOrElse(throw new Exception("id was None when trying to convert object to id"))
}
