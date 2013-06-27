package models
package object types{
  import slick.lifted.{MappedTypeMapper}
  import util.schema._

  // ENUM example
  sealed abstract class Size
  object Size{
    case object Small extends Size
    case object Medium extends Size
    case object Large extends Size
    implicit val sizeTypeMapper = {
      val forward = Map[Size,Int](
        Small  -> 1,
        Medium -> 2,
        Large  -> 3
      )
      val backwards = forward map {_.swap}
      MappedTypeMapper.base[Size, Int](
        s => forward.getOrElse  ( s, throw new Exception("cannot map $s to Int") ),
        i => backwards.getOrElse( i, throw new Exception("cannot map $i to Size") )
      )
    }
  }

  // Date mapper
  implicit val javaUtilDateTypeMapper = MappedTypeMapper.base[java.util.Date, java.sql.Date](
    x => new java.sql.Date(x.getTime),
    x => new java.util.Date(x.getTime)
  )

  // typed id value classes
  //sealed trait TypedId extends util.schema.TypedId
  case class CompanyId       (val id: Long) extends AnyVal with TypedId
  case class ComputerId      (val id: Long) extends AnyVal with TypedId
  case class DeviceId        (val id: Long) extends AnyVal with TypedId
  case class SiteId          (val id: Long) extends AnyVal with TypedId
  case class ResearchSiteId  (val id: Long) extends AnyVal with TypedId
  case class ProductionSiteId(val id: Long) extends AnyVal with TypedId

  sealed trait IdFactory[T <: TypedId] extends (Long => T)

  implicit object CompanyId extends IdFactory[CompanyId]
  implicit object ComputerId extends IdFactory[ComputerId]
  implicit object DeviceId extends IdFactory[DeviceId]
  implicit object SiteId extends IdFactory[SiteId]
  implicit object ResearchSiteId extends IdFactory[ResearchSiteId]
  implicit object ProductionSiteId extends IdFactory[ProductionSiteId]


  // typed id type mappings
  import MappedTypeMapper.{base=>mapType}
  implicit val companyIdMapper        = mapType[CompanyId       , Long](_.id,CompanyId)
  implicit val computerIdMapper       = mapType[ComputerId      , Long](_.id,ComputerId)
  implicit val deviceIdMapper         = mapType[DeviceId        , Long](_.id,DeviceId)
  implicit val siteIdMapper           = mapType[SiteId          , Long](_.id,SiteId)
  implicit val researchSiteIdMapper   = mapType[ResearchSiteId  , Long](_.id,ResearchSiteId)
  implicit val productionSiteIdMapper = mapType[ProductionSiteId, Long](_.id,ProductionSiteId)

  implicit def longToId[T <: TypedId](id:Long)( implicit create : IdFactory[T] ) = create(id)
  implicit def longToIdOption[T <: TypedId](id:Long)( implicit create : IdFactory[T] ) = Option(create(id))
  implicit def longToId[T <: TypedId](id:Option[Long])( implicit create : IdFactory[T] ) = id.map(create)


  // play custom id formatters
  object LongEx {
    def unapply(s : String) : Option[Long] = try {
      Some(s.toLong)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

  import play.api.data.format.Formatter

  implicit def idFormatter[T <: TypedId](implicit create : IdFactory[T]) : Formatter[T] = new Formatter[T] {
    override val format = Some(("format.id", Nil))

    def bind(key: String, data: Map[String, String]) = {
      Right(data.get(key).getOrElse("false")).right.flatMap {
        case LongEx(i) => Right( create(i) )
        case _ => Left(Seq(play.api.data.FormError(key, "error.id", Nil)))
      }
    }

    def unbind(key: String, value: T) = Map(key -> value.toString)
  }
}