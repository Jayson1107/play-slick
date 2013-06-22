package models
import play.api.db.slick.Config.driver.simple._

import play.api.cache.Cache
import play.api.db.slick.DB
import play.api.Play.current

// models dependencies
import entities._
import autojoin._
import relationships._
import queries._
import tables._

/**
  exists, so DAO can be imported as plain names (without DAO postfix)
*/
object dao{
  import DAOWrapper._
  val Companies       = CompaniesDAO
  val Computers       = ComputersDAO
  val Devices         = DevicesDAO
  val Sites           = SitesDAO  
  val ProductionSites = ProductionSitesDAO
  val ResearchSites   = ResearchSitesDAO
  val byTable = Map[BaseTable[_],DAOBase[_ <: Product]](
    schema.Companies -> CompaniesDAO
  )
}
/**
  DAO objects are postfixed with "DAO" so tables can be imported into scope with plain name.
  The DAO object's methods require implicit Sessions instead or creating them, so they can be composed in a single session.
 */
object DAOWrapper{
  abstract class DAOBase[Entity <: entities.Entity]{
    type TableType <: Table[Entity] with BaseTable[Entity]
    def table : TableType
    def query = Query(table)

    protected trait PreparedBase{
      val byId   = Parameters[Long].flatMap(query byId _)
                // Parameters[(Long,Long)].flatMap( (query someFunc _).tupled )
      val length = query.length
    }

    private object Prepared extends PreparedBase

    /**
     * Retrieve a computer from the id
     * @param id
     */
    def byId(id: Long)(implicit s:Session): Option[Entity] = {
        Prepared.byId(id).firstOption
    }

    /**
     * Count all computers
     */
    def length()(implicit s:Session) = {
      Prepared.length.run
    }

    def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%")(implicit s:Session): Page[Entity] = {
      val filteredQuery = query
      val offset = pageSize * page
      val total = filteredQuery.length.run // System.currentTimeMillis() / 1000
      import scala.slick.lifted._
      val values =
          filteredQuery
          // add filter
          .sortByRuntimeValue(_.column _, orderBy)
          .paginate(page,pageSize)
          .run
      Page(values, page, pageSize, offset, total )
    }


    /**
     * Insert an entity
     * @param entity
     */
    def insert(entity: Entity)(implicit s:Session) {
      table.autoInc.insert(entity)
    }
    /**
     * Update an entity
     * @param query
     * @param entity
     */
    def update(query: Query[TableType,Entity], value: Entity)(implicit s:Session) {
      query.update( value ) // id may be overridden in this implementation
    }
    /**
     * Delete an entity
     * @param query
     */
    def delete(query: Query[TableType,Entity])(implicit s:Session) {
      query.delete
    }
  }
  object SitesDAO extends DAOBase[Site]{
    type TableType = schema.Sites
    def table = schema.Sites
  }
  object ProductionSitesDAO extends DAOBase[ProductionSite]{
    type TableType = schema.ProductionSites
    def table = schema.ProductionSites
  }
  object ResearchSitesDAO extends DAOBase[ResearchSite]{
    type TableType = schema.ResearchSites
    def table = schema.ResearchSites
  }
  object DevicesDAO extends DAOBase[Device]{
    type TableType = schema.Devices
    def table = schema.Devices
  }
  object CompaniesDAO extends DAOBase[Company]{
    type TableType = schema.Companies
    def table = schema.Companies
    private object Prepared extends PreparedBase{}
  }
  object ComputersDAO extends DAOBase[Computer]{
    type TableType = schema.Computers
    def table = schema.Computers
    private object Prepared extends PreparedBase{}

    /**
     * Return a page of (Computer,Company)
     * @param page
     * @param pageSize
     * @param orderBy
     * @param pattern
     */
    def withCompanies(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%"): Page[(Computer, Option[Company])] = {
      Cache.getOrElse(s"DAO.Computers.list($page,$pageSize,$orderBy,$filter)",5){
        DB.withTransaction{ implicit session =>
          val computers = Computers.byName(filter)
          val offset = pageSize * page
          val total = computers.length.run
          import scala.slick.lifted._
          val values =
            computers
              .join(Companies)
              .map{
                case (computer, company) => (computer, company.id.?, company.name.?)
              }
              .sortByRuntimeValue(
                r => Map(
                  1 -> r._1.name,
                  2 -> r._1.introduced,
                  3 -> r._1.discontinued,
                  4 -> r._3
                ),
                orderBy
              )
              .paginate(page,pageSize)
              .run
              .map(
                row => (
                  row._1,
                  row._2.map( _ => Company(row._2,row._3.get) )
                )
              )
          Page(values, page, pageSize, offset, total )
        }
      }
    }
  }
}

/// PREPARE tries


  /*
    import scala.slick.lifted.Shape
    def prepare[T:TypeMapper,QU](f: Column[T] => Query[_, QU] ) = Parameters[T](Shape.unpackPrimitive[T]).flatMap( f )


    protected trait PreparedBase{
      val byId   = prepare( (c:Column[Long]) => query.byId(c) )
    }
  /*  def prepare
      [T:TypeMapper,RT:TypeMapper,QU]
      (f: ((Column[T],Column[RT])) => Query[_, QU] )
      (implicit shape: Shape[(T,RT), (T,RT), (Column[T],Column[RT])])
      = Parameters[(T,RT)].flatMap( f )*/
    def prepare
      [T,QU,PP]
      (f: PP => Query[_, QU] )
      (implicit shape: Shape[T, T, PP])
      = Parameters[T].flatMap( f )
    //def prepare[T:TypeMapper,PP,QU](f: PP => Query[_, QU] )/*(implicit shape: Shape[T, T, PP])*/ = Parameters[T].flatMap( f )
    def byIdPrepared4 = prepare( byId _ )//(Shape.unpackPrimitive[Long])

    def byId2( id:scala.slick.lifted.Column[Long], id2:scala.slick.lifted.Column[Long] ) = {
  //    def id_ = Some(id)
      Query(this).filter(_.id === id).filter(_.id === id2).map(x=>x)
    }
    def byIdPrepared2 = Parameters[(Long,Long)].flatMap( (byId2 _).tupled )
    def byIdPrepared3[T](implicit i:Shape[T,T,(Column[Long],Column[Long])])
     = prepare( (byId2 _).tupled )
  */
