package util
import play.api.db.slick.Config.driver.simple._

package object queries{
  // generic column helpers (easier as functions rather than extension methods)
  def iLike( lhs:Column[String], rhs:Column[String] ) = lhs.toLowerCase like rhs.toLowerCase
}
