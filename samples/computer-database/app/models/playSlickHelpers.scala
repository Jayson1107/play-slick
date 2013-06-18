package models

import play.api.db.slick.Config.driver.simple._

// slick dependencies
import slick.ast.{JoinType}

// remove with TableTrait
import slick.lifted.{ColumnOption,Index,Shape,ForeignKeyQuery,ForeignKeyAction} 
//,TypeMapper,MappedTypeMapper,Join, MappedTypeMapper, MappedProjection, Projection}
import slick.ast.{TableNode}

object playSlickHelpers{
  // autojoin
  class AutoJoin[LE,RE,LT,RT]( val autoJoin : (Query[LT,LE],Query[RT,RE],JoinType) => Query[(LT,RT),(LE,RE)] ) extends ((Query[LT,LE],Query[RT,RE],JoinType) => Query[(LT,RT),(LE,RE)]){
    that =>
    def apply(l:Query[LT,LE],r:Query[RT,RE],joinType:JoinType = JoinType.Inner) = autoJoin(l,r,joinType)
  }
  class JoinCondition[-L,-R]( val condition : (L,R) => Column[Boolean] ) extends ((L,R) => Column[Boolean]){
    that =>
    def apply(l:L,r:R) = condition(l,r)
  }
  class ReversableJoinCondition[-L,-R]( condition : (L,R) => Column[Boolean] ) extends JoinCondition(condition){    
    def reversed = new JoinCondition[R,L]( (r,l) => apply(l,r) )
  }
  def joinCondition[L,R]( condition : (L,R) => Column[Boolean] ) = new ReversableJoinCondition(condition)
  def complexAutoJoin[LE,RE,LT,RT]( autoJoin : (Query[LT,LE],Query[RT,RE],JoinType) => Query[(LT,RT),(LE,RE)] ) = new AutoJoin(autoJoin)
  object implicits{
    // autojoin
    implicit def fetchAutoJoin[LE,RE,LT <: Table[LE],RT <: Table[RE]](implicit condition:JoinCondition[LT,RT])
      = new AutoJoin( (l:Query[LT,LE],r:Query[RT,RE],joinType:JoinType) => (l.join(r, joinType)) on condition )

    //implicit def autoReversedJoin[L,R](implicit joinCondition : JoinCondition[R,L]) = joinCondition.reversed
    implicit def autoReversedJoinCondition[L,R](implicit joinCondition : ReversableJoinCondition[R,L]) = joinCondition.reversed
    implicit def extendAllQueries[LE,LT](q:Query[LT,LE]) = new {
      def autoJoin[RE,RT]( target:Query[RT,RE], joinType:JoinType = JoinType.Inner )( implicit autoJoin : AutoJoin[LE,RE,LT,RT] )
          = autoJoin( q, target, joinType )
      def autoJoinVia2[RE,RT,RMT,LMT]( target:Query[RT,RE], joinType:JoinType = JoinType.Inner )(leftVia:LT => LMT,rightVia:RT => RMT)( implicit joinCondition : JoinCondition[LMT,RMT] )
          = {
            q.join(target,joinType)
              .on( (l,r) => {
                println((l,r))
                joinCondition(leftVia(l),rightVia(r))
              })
          }
    }
    object more{
      implicit def extendJoinedTableQueries[LE,LT <: Table[LE],RE,RT <: Table[RE]](q:Query[(LT,RT),(LE,RE)]) = new {
        def further[RRE,RRT <: Table[RRE]]( target:Query[RRT,RRE], joinType:JoinType = JoinType.Inner )( implicit joinCondition : JoinCondition[RT,RRT] )
          = extendAllQueries(q).autoJoinVia2(target,joinType)(_._2,x=>x).map(r => (r._1._1,r._2))
        def autoJoinVia[RRE,RRT<:Table[RRE],MT]( target:Query[RRT,RRE], joinType:JoinType = JoinType.Inner )(via:((LT,RT)) => MT)( implicit joinCondition : JoinCondition[MT,RRT] )
          = extendAllQueries(q).autoJoinVia2(target,joinType)(via,x=>x)
      }
      implicit def extendTableQueries[LE,LT <: Table[LE]](q:Query[LT,LE]) = new {
        def autoJoin[RE,RT <: Table[RE]]( target:Query[RT,RE], joinType:JoinType = JoinType.Inner )( implicit autoJoin : AutoJoin[LE,RE,LT,RT] )
            = autoJoin( q, target, joinType )
/*        def autoJoinVia[RE,RT,MT]( target:Query[RT,RE], joinType:JoinType = JoinType.Inner )(via:RT => MT)( implicit joinCondition : JoinCondition[LT,MT] )
            = extendAllQueries(q).autoJoinVia2(target,joinType)(x=>x,via)
        def further[RE,RT <: Table[RE],ME,MT <: Table[ME]]( target:Query[(MT,RT),(ME,RE)], joinType:JoinType = JoinType.Inner )( implicit joinCondition : JoinCondition[LT,MT] )
          = autoJoinVia(target,joinType)(_._1).map(r => (r._1,r._2._2))
*/
      }
    }
/*    implicit def extendJoinedQuery[LE,LT,ME,MT](q:Query[(LT,MT),(LE,ME)]) = new {
      def autoJoin[RE,RT]( target:Query[RT,RE], joinType:JoinType = JoinType.Inner )( implicit autoJoin : AutoJoin[LE,RE,LT,RT] )
          = {
      }autoJoin( q, target, joinType )

      val cond4 = implicitly[JoinCondition[Sites,Devices]]
      .join(computers, joinType).on( (si,c) => cond1(si._2,c) ).map({ case ((s,i),c) => (s,c) })
    }*/
  }



/*  abstract class SlickBaseTable[E](table:String) extends Table[E](table){
    //def getTable[RE,RT <: Table[RE]]( t: RT, joinType:JoinType = JoinType.Inner )(implicit joinCondition:JoinCondition[this.type,RT]) : Query[RT,RE]
    //  = get( Query(t), joinType )
    //def get[RE,RT <: Table[RE]]( q: Query[RT,RE], joinType:JoinType = JoinType.Inner )(implicit joinCondition:JoinCondition[this.type,RT]) : Query[RT,RE]
    //  = q.filter( r => joinCondition(this,r) )
  }*/
}
