package models
package object queries{
  import play.api.db.slick.Config.driver.simple._

  import slick.ast.{JoinType}
  import slick.lifted.{BaseTypeMapper}

  import util.autojoin._
  import util.schema._

  import entities._
  import schema.interfaces._
  import relationships._
  import tables._

  implicit def extendHasId[E,T <: HasId](q:Query[T,E]) = new{
    import q._
    def byId( id:Column[Long] )
      = filter(_.untypedId === id)
//    def insert[E]( entity:E )(implicit session:Session,table:BaseTable[E]) = schema.autoInc.insert(entity)
  }
  implicit def extendHasTypedId[E,T <: HasTypedId](q:Query[T,E]) = new{
    import q._
    def byTypedId[ID <: T#IdType : BaseTypeMapper]( typedId:Column[ID] )
      = filter(r => typedId === r.typedId.asInstanceOf[ID])
  }
  implicit def extendAllQueries3[E,T](q:Query[T,E]) = new{
    import q._
    def paginate( page: Int , pageSize : Int )
      = drop(pageSize * page)
        .take(pageSize)
    def sortByRuntimeValue( columns:T => Int => Column[_], index:Int )
      = sortBy{ r =>
      val cond = columns(r)(index.abs).nullsLast
      if(index > 0) cond else cond.desc
    }
  }

  implicit def extendHasName[E,T <: HasName with HasId](q:Query[T,E]) = new {
    import q._
    def byName( pattern:Column[String] )
      = filter(_.byName(pattern))
    /**
     * Construct the Map[String,String] needed to fill a select options set
     */
    def options = sortBy(_.name).map( c => (c.untypedId.asColumnOf[String],c.name) )
  }

  //TODO: make Query extension
  def withChildren(sites:Query[schema.Sites,Site]) =
    sites.autoJoin( ResearchSites, JoinType.Left )
     .autoJoinVia( ProductionSites, JoinType.Left )(_._1)
     .map{ case((s,r),p) => (s, r.size.?, p.volume.?) }
  
/*
  implicit def extendBaseTableBlind[_,T <: Table[_]](t1:T) = new{
    import t1._
    def getTable[RE,RT <: Table[RE]]( t: RT, joinType:JoinType = JoinType.Inner )(implicit joinCondition:JoinCondition[T,RT]) : Query[RT,RE]
      = get( Query(t), joinType )
    def get[RE,RT <: Table[RE]]( q: Query[RT,RE], joinType:JoinType = JoinType.Inner )(implicit joinCondition:JoinCondition[T,RT]) : Query[RT,RE]
      = q.filter( r => joinCondition(t1,r) )
  }
*/
}
