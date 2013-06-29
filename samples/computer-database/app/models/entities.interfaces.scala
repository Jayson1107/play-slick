package models.entities.interfaces
import util.schema._
trait Entity
trait HasUntypedId extends Entity{
  def untypedId: Option[Long]
}
trait HasTypedId[T <: TypedId] extends HasUntypedId{
  type IdType = T
  def untypedId = typedId.map(_.untypedId)
  def typedId: Option[T]
}
trait HasName extends Entity{
  def name: String
}