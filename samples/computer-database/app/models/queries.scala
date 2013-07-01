package models
package object queries{
  import play.api.db.slick.driver.simple._

  import slick.ast.{JoinType}
  import slick.lifted.{BaseTypeMapper}

  import util.autojoin._
  import util.schema._

  import entities._
  import schema.interfaces._
  import relationships._
  import tables._

  // aliases for shorter notation
  type C[Type]          = Column[Type]
  type Q[Table,Element] = Query[Table,Element]
  type BTM[Type]        = BaseTypeMapper[Type]
  implicit def asInstanceOfAsAs( a:Any ) = new {
    def as[Type] = a.asInstanceOf[Type]
  }

  // Query extension methods
  implicit def extendUntypedId[E,T <: HasUntypedId]( q : Q[T,E] ) = new{
    def byUntypedId( untypedId : C[Long] ) = q.filter( _.untypedId === untypedId )
  }
  implicit def extendTypedId[E,T <: HasId]( q : Q[T,E] ) = new{
    def byTypedId[Id >: T#Id <: T#Id : BTM]( id:C[Id] ) = q.filter(_.id.as[C[Id]] === id)
  }
  implicit def extendAll[E,T]( q : Q[T,E] ) = new{
    def paginate( page: Int, pageSize : Int ) = q.drop( pageSize * page ).take( pageSize )
    def sortByRuntimeValue( columns:T => Int => C[_], index:Int ) =
      q.sortBy{ r =>
        val cond = columns(r)(index.abs).nullsLast
        if(index > 0) cond else cond.desc
      }
  }

  implicit def extendHasName[E,T <: HasName with HasUntypedId](q:Q[T,E]) = new {
    def byName( pattern:C[String] ) = q.filter( _.byName(pattern) )
    /**
     * Construct the Map[String,String] needed to fill a select options set
     */
    def options = q.sortBy(_.name).map( c => (c.untypedId.asColumnOf[String],c.name) )
  }

  //TODO: make Query method extension
  def withChildren(sites:Q[schema.Sites,Site]) =
    sites.autoJoin( ResearchSites, JoinType.Left )
     .autoJoinVia( ProductionSites, JoinType.Left )(_._1)
     .map{ case((s,r),p) => (s, r.size.?, p.volume.?) }
  
}
