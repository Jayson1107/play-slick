package util
package object queries{
   import play.api.db.slick.driver.simple._

  // generic column helpers (easier as functions rather than extension methods)
  def iLike( lhs:Column[String], rhs:Column[String] ) = lhs.toLowerCase like rhs.toLowerCase
}
