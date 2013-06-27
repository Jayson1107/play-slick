package models.entities.interfaces
import util.schema._
trait Entity
trait HasId extends Entity{
  def id: Option[Long]
}
trait HasTypedId[T <: TypedId] extends HasId{
  type IdType = T
  def id = typedId.map(_.id)
  def typedId: Option[T]
}
trait HasName extends Entity{
  def name: String
}