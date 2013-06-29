package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.db.slick._
import play.api.Play.current
import play.api.db.slick.driver.simple.{Session => DBSession,_}

import views._

import util.autojoin._

import models._
import models.dao
import models.entities._
import models.idConversions._
import models.queries._
import models.relationships._
import models.tables._
import models.types._

import slick.ast.{JoinType}

/**
 * Manage a database of computers
 */
object Application extends Controller { 
  import play.templates.TemplateMagic._
 /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Application.list("computers",0, 1, ""))
  
  /**
   * Describe the computer form (used in both edit and create screens).
   */ 
  val computerForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "introduced" -> optional(date("yyyy-MM-dd")),
      "discontinued" -> optional(date("yyyy-MM-dd")),
      "company" -> optional(of[CompanyId]),
      "id" -> optional(of[ComputerId])
    )(Computer.apply)(Computer.unapply)
  )
  
  // -- Actions

  /**
   * Handle default path requests, redirect to computers list
   */  
  def index = Action { Home }


  def examples(name:String) = DBAction{ implicit request =>
    def show(values:Any) = Ok(html.main(html.show(values)))
    // see slides
    name match{
      case "collectionLikeApi" =>
        // for-comprehension Syntax
        val q = for ( d <- Devices;
          if d.price > 1000.0
        ) yield d.acquisition
        show( q.run.map(_.format("yyyy-MM-dd")) )

      case "collectionLikeApi2" =>
        // method-based collection syntax (which for-comprehensions expand to internally)
        val q = Devices
                .filter(_.price > 1000.0)
                .map(_.acquisition)
        show( q.run.map(_.format("yyyy-MM-dd")) )

      case "minimalConfiguration" => {
        import play.api.db.slick.driver.simple.Session
        import play.api.db.slick.driver.simple.Database
        // describe an in-memory h2 database, which is deleted after each session/connection
        // use jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1 to keep it for several sessions until the jvm end
        val db = Database.forURL("jdbc:h2:mem:test1", "org.h2.Driver")

        // an automatically closing session scope, which lazily opens a connection on first use and
        // closes it or returns it to the connection pool on exiting the scope
        db.withSession {
          implicit s: Session =>
          // <- queries here
        }

        // the same but not as a transaction
        db.withTransaction{
          implicit s: Session =>
          // <- queries here
        }
        show("nothing here")
      }

      case "cleanDataModel" => {
        import java.sql.Date
        case class Device(
          id: Long,
          price: Double,
          acquisition: Date
        )
        class Devices extends Table[Device]("DEVICE") {
          def id = column[Long]("ID", O.PrimaryKey)
          def price = column[Double]("PRICE")
          def acquisition = column[Date]("ACQUISITION")
          def * = id ~ price ~ acquisition <>
            (Device.apply _, Device.unapply _)
        }
        val Devices = new Devices
        show(Query(Devices).map(d => (d.id,d.price)).run)
      }

      case "noDataModel" => {
        import java.sql.Date
        class Devices extends Table[(Long, Double, Date)]("DEVICE") {
          def id = column[Long]("ID", O.PrimaryKey)
          def price = column[Double]("PRICE")
          def acquisition = column[Date]("ACQUISITION")
          def * = id ~ price ~ acquisition
        }
        val Devices = new Devices
        show(Query(Devices).map(d => (d.id,d.price)).run)
      }

      case "customColumnTypes" => {
        case class Device(id: DeviceId,price:Double)
          
        implicit val deviceIdType = MappedTypeMapper.base
          [DeviceId, Long](_.untypedId, new DeviceId(_))
          
        class Devices extends Table[Device]("DEVICE") {
          def id = column[DeviceId]("ID", O.PrimaryKey)
          def price = column[Double]("PRICE")
          def * = id ~ price <> (Device.apply _, Device.unapply _)
        }
        val Devices = new Devices
        show(Query(Devices).map(d => (d.id,d.price)).run)
      }

      case "transferredData" => {
        // explicit control over execution and transfer: 2 queries, device not fetched from db
        val device = Devices.byUntypedId(123L) : Query[schema.Devices,Device] 
        val site   = Site("New York")
        val siteId = schema.tables.Sites.autoIncTypedId.insert( site )
        device.map(_.siteId).update(siteId)
        show("updates done")
      }
      case "selectStatement" => {
        // show generated SQL code for debugging
        val sql = Devices
          .filter(_.price > 1000.0)
          .map(_.acquisition)
          .selectStatement
        show(sql)
      }
      case "joins" => {
        // simple join with Slick's built-in join on methods

        // put condition into a val so it can be re-used
        val sitesToDevices = (s:schema.Sites,i:schema.Devices) => s.id === i.siteId 

        // just two queries
        val sites   = Sites.filter(_.untypedId === 1L)
        val devices = Devices.filter(_.price > 1000.0)

        // join the two queries using the condition
        val res1 = sites.join( devices ).on( sitesToDevices ) run

        // scala's alternative method call syntax without . and ( )
        val res2 = sites join devices on sitesToDevices run

        show(res2)
      }
      case "autojoins-1-n" => {
        // autojoins, which are not a Slick feature, but implemented in oackage playSlickHelpers of this sample app
        implicit def autojoin1 = joinCondition(Sites,Devices)(_.id === _.siteId)

        // just two queries
        val sites   = Sites.filter(_.untypedId != 1L)
        val devices = Devices.filter(_.price > 0.0)

        // inner join
        val res1 = sites autoJoin devices run

        // inner join in opposite direction
        val res2 = devices autoJoin sites run

        // left outer join selecting particular fields from the joined table using the .? operator to avoid "Read NULL value for column"
        val res3 = sites.autoJoin( devices, JoinType.Left )
                       .map{ case (s,d) => (s, (d.price.?,d.acquisition.?)) }
                       .run

        // outer join fetching whole objects (using .option helper method for mapping)
        val res4 = sites.autoJoin( devices, JoinType.Left )
                       .map( row => (row._1,row._2.?) ) // .? is a user defined method in table Devices
                       .run

        Ok(html.main(html.show(res4.mkString("\n"))))
      }
      case "joinTypes" => {
        val sitesToDevices = (s:schema.Sites,i:schema.Devices) => s.id === i.siteId 
        val sites = Sites
        val devices = Devices
        sites leftJoin  devices on sitesToDevices
        sites rightJoin devices on sitesToDevices
        sites outerJoin devices on sitesToDevices

        sites.autoJoin( devices, JoinType.Left )
        sites.autoJoin( devices, JoinType.Right )
        sites.autoJoin( devices, JoinType.Outer )

        Ok(html.main(html.show("nothing here")))
      }
      case "autojoins-n-n" => {
        implicit def autojoin1 = joinCondition(Sites,Devices)(_.id === _.siteId)
        implicit def autojoin2 = joinCondition(Devices,Computers)(_.computerId === _.id)

        val q = Sites.autoJoin(Devices).further(Computers) : Query[_,(Site,Computer)]
        Sites.autoJoin(Devices).autoJoinVia(Computers)(_._2) : Query[_,((Site,Device),Computer)]
        Ok(html.main(html.show(q.run)))
      }
      case "subClasses" => {
        val q = Sites
         .autoJoin( ResearchSites, JoinType.Left )
         .autoJoinVia( ProductionSites,  JoinType.Left )(_._1)
         : Query[_,((Site,ResearchSite),ProductionSite)]
        Ok(html.main(html.show("nothing here")))
      }
      case "rowFunctions" => {
        val q = Sites.filter( _.byName("Lausanne") )
        Ok(html.main(html.show(q.run)))
      }
      case "filterNone" => {
        val res = Computers.filter(_.companyId isNotNull).map(_.companyId.get).run
        println(res)
        show((res))
      }
/*
      case "dynamicColumns" => {
        val columns = Seq("id","name")
        val q = Sites.map( r => slick.util.TupleSupport.buildTuple( columns.map( name => r.column[String](name) ) ) )
        Ok(html.main(html.show(q.run)))
      }
*/
      case "" => {
        val q = Sites
        Ok(html.main(html.show(q.run)))
      }
      case "sites" =>

        def foo( s:Query[schema.Sites,Site], i:Query[schema.Devices,Device] ) = s autoJoin i
import slick.jdbc.StaticQuery.interpolation
val price = 1000.0
/*        println(
sql"""
  select *
  from ITEMS
  where price > $price
""".as[Device]
)*/
        (Sites.filter(_.untypedId === 1L).autoJoin(Devices.filter(_.price > 1000.0),JoinType.Left)).run
        for(
          i <- Devices;
          s <- i.site;
          if s.untypedId === 1L && i.price > 1000.0
        ) yield (i,s)



        val q = Sites.autoJoin( Computers, JoinType.Left )
          .map{ case(s,c) => (s.name,c.name.?) }
          println(q.selectStatement)
        val res = q.run
          .groupBy(_._1)
          .mapValues(_.flatMap(_._2))
          .map{ case(s,c) => (s,c.mkString(", ")) }
          .map(_.productIterator.map(s => Some(s.toString)).toList).toList

        // Stefan fragen: [JdbcSQLException: Column "X11.id" not found; SQL statement: CREATE FORCE VIEW PUBLIC._25 AS SELECT X11."id" AS X3, X11."name" AS X4 FROM SYSTEM_RANGE(1, 1) [42122-168]]
        // val res = Sites.flatMap(s => Query(s) autoJoin Devices).run

        Ok(html.main(html.table("sites",Seq(),res)))
    }
  }
  /**
   * Display the paginated list of computers.
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on computer names
   */
  def list(tableName:String, page: Int, orderBy: Int, filter: String) = DBAction { implicit rs =>
    val args = schema.byName(tableName) match{
      case schema.tables.Computers =>
        val currentPage = dao.Computers.withCompanies(page = page, orderBy = orderBy, filter = ("%"+filter+"%"))
        (
          currentPage,
          Seq(
              (1, "Computer name"),
              (2, "Introduced"),
              (3, "Discontinued"),
              (4, "Company")
          ),
          currentPage.items.map { case (computer,company) =>
              Seq(
                  Some(<a href={routes.Application.edit(computer.id.get.untypedId).toString}>{computer.name}</a>),
                  computer.introduced.map(_.format("dd MMM yyyy")),
                  computer.discontinued.map(_.format("dd MMM yyyy")),
                  company.map(_.name)
              )
          }
        )
      case t => 
        val currentPage = dao.byTable(t).list(page = page, orderBy = orderBy, filter = ("%"+filter+"%"))
        (
          currentPage,
          Seq(
              
          ),
          currentPage.items.map {_.productIterator.drop(1).map(e => Some(e.toString)).toSeq}
        )
    }
    Ok(
      ((currentPage:Page[_],headers:Seq[(Int,String)],values:Seq[Seq[Option[java.io.Serializable]]])
          => html.list.apply(currentPage,orderBy,filter,tableName,headers,values)).tupled(args)
    )
  }
  
  /**
   * Display the 'edit form' of a existing Computer.
   *
   * @param untypedId Id of the computer to edit
   */
  def edit(untypedId: Long) = DBAction { implicit rs =>
    dao.Computers.byUntypedId(untypedId).map { computer =>
      Ok(html.editForm(untypedId, computerForm.fill(computer), Companies.options.run))
    }.getOrElse(NotFound)
  }
  
  /**
   * Handle the 'edit form' submission 
   *
   * @param untypedId Id of the computer to edit
   */
  def update(untypedId: Long) = DBAction { implicit rs =>
    computerForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.editForm(untypedId, formWithErrors, Companies.options.run)),
      computer => {
        dao.Computers.update( untypedId, computer ) // TODO: queries.Computers.byUntypedId(id).update( computer ) // TODO: what happens if we don't set id manually?
        Home.flashing("success" -> "Computer %s has been updated".format(computer.name))
      }
    )
  }
  
  /**
   * Display the 'new computer form'.
   */
  def create(kind:String) = DBAction { implicit rs =>
    kind match{
      case "computers" => Ok(html.createForm(computerForm, Companies.options.run))
    }
  }
  
  /**
   * Handle the 'new computer form' submission.
   */
  def save = DBAction { implicit rs =>
    computerForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.createForm(formWithErrors, Companies.options.run)),
      computer => {
        dao.Computers.insert( computer ) // TODO: replace by queries.Computers.insert(computer)queries.Computers.insert(computer)(implicitly[DBSession],tables.Computers)
        Home.flashing("success" -> "Computer %s has been created".format(computer.name))
      }
    )
  }
  
  /**
   * Handle computer deletion.
   */
  def delete(untypedId: Long) = DBAction { implicit rs =>
    dao.Computers.delete(untypedId) // TODO: replace by queries.Computers.byUntypedId(id).delete aka queryToDeleteInvoker(queries.Computers.byUntypedId(id)).deleteInvoker.delete
    Home.flashing("success" -> "Computer has been deleted")
  }

}
            
