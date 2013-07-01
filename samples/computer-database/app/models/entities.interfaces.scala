package models.entities.interfaces
import util.schema._
trait Entity
trait HasUntypedId extends Entity{
  def untypedId: Option[Long]
}
trait HasId[T <: TypedId] extends HasUntypedId{
  type Id = T
  def untypedId = id.map(_.untypedId)
  def id: Option[T]
}
trait HasName extends Entity{
  def name: String
}