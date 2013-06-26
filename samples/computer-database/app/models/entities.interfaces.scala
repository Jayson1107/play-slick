package models.entities.interfaces
trait Entity
trait HasId extends Entity{
  def id: Option[Long]
}
trait HasName extends Entity{
  def name: String
}