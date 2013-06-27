package models.entities.interfaces
import util.schema._
trait Entity
trait HasId[T <: TypedId] extends Entity{
  type IdType = T
  def id: Option[Long]
}
trait HasName extends Entity{
  def name: String
}