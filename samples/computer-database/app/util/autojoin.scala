package util
/**
 * Implements as autojoin feature, which is not part of Slick at the moment.
 * Also see http://slick.typesafe.com/talks/2013_scaladays/2013_scaladays.pdf
 */
package object autojoin{
  import play.api.db.slick.Config.driver.simple._ // FIXME: change into dynamic dependency

  // slick dependencies
  import slick.ast.JoinType
  import slick.lifted.CanBeQueryCondition

  /** executes a perticular join */
  class AutoJoin[LE,RE,LT,RT]( val doJoin : JoinType => (Query[LT,LE],Query[RT,RE]) => Query[(LT,RT),(LE,RE)] ) extends (JoinType => (Query[LT,LE],Query[RT,RE]) => Query[(LT,RT),(LE,RE)]){
    def apply(joinType:JoinType) = new Function2[Query[LT,LE],Query[RT,RE],Query[(LT,RT),(LE,RE)]]{
      def apply(l:Query[LT,LE],r:Query[RT,RE]) = doJoin(joinType)(l,r)
    }
  }

  /** describes a join condition, which can be reversed for bi-directional use */
  class ReversableJoinCondition[-L,-R]( condition : (L,R) => Column[Boolean] ) extends JoinCondition(condition){    
    def reversed = new JoinCondition[R,L]( (r,l) => apply(l,r) )
  }

  /** implicitly reverses a join condition when needed */
  implicit def autoReversedJoinCondition[L,R](implicit joinCondition : ReversableJoinCondition[R,L]) = joinCondition.reversed

  /** a join condition which cannot be reversed again (to avoid endless recursion with implicit reversing) */
  class JoinCondition[-L,-R]( val condition : (L,R) => Column[Boolean] ) extends ((L,R) => Column[Boolean]){
    def apply(l:L,r:R) = condition(l,r)
  }
  
  /** An explicit converter from a table interface type to a query which can be used for type hinting in join conditions  */
  def InterfaceJoin[T] : Query[T,_] = null // TODO: make this safer using type class

  /** Creates a join condition which can be used for autojoins, when stored in an implicit val or def
    * The first argument is only used to guide type inference, the actual values are ignored.
    * To describe a join with an table interface use InferfaceJoin
    */
  def joinCondition[L,R,T <: Column[_] : CanBeQueryCondition](l:Query[L,_],r:Query[R,_])( condition : (L,R) => T ) = new ReversableJoinCondition(condition.asInstanceOf[(L,R) => Column[Boolean]])

  /** Creates an AutoJoin that stores how to join the two given tables possibly spanning several intermediate tables
    * The first argument is only used to guide type inference, the actual values are ignored.
    * To describe a join with an table interface use InferfaceJoin
    */
  def complexJoin[LE,RE,LT,RT](l:Query[LT,LE],r:Query[RT,RE])( doJoin : JoinType => (Query[LT,LE],Query[RT,RE]) => Query[(LT,RT),(LE,RE)] ) = new AutoJoin(doJoin)

  /** automaticall converts an implicitly available JoinCondition to an AutoJoin, which can be used by autoJoin */
  implicit def fetchAutoJoin[LE,RE,LT <: Table[LE],RT <: Table[RE]](implicit condition:JoinCondition[LT,RT])
    = new AutoJoin( (joinType:JoinType) => (l:Query[LT,LE],r:Query[RT,RE]) => (l.join(r, joinType)) on condition )


  implicit def extendAllQueries[LE,LT](q:Query[LT,LE]) = new {
    def autoJoin[RE,RT]( target:Query[RT,RE], joinType:JoinType = JoinType.Inner )( implicit doJoin : AutoJoin[LE,RE,LT,RT] )
        = doJoin(joinType)( q, target )
    def autoJoinVia2[RE,RT,RMT,LMT]( target:Query[RT,RE], joinType:JoinType = JoinType.Inner )(leftVia:LT => LMT,rightVia:RT => RMT)( implicit joinCondition : JoinCondition[LMT,RMT] )
        = {
          q.join(target,joinType)
            .on( (l,r) => {
              println((l,r))
              joinCondition(leftVia(l),rightVia(r))
            })
        }
  }
  implicit def extendJoinedTableQueries[LE,LT <: Table[LE],RE,RT <: Table[RE]](q:Query[(LT,RT),(LE,RE)]) = new {
    def further[RRE,RRT <: Table[RRE]]( target:Query[RRT,RRE], joinType:JoinType = JoinType.Inner )( implicit joinCondition : JoinCondition[RT,RRT] )
      = extendAllQueries(q).autoJoinVia2(target,joinType)(_._2,x=>x).map(r => (r._1._1,r._2))
    def autoJoinVia[RRE,RRT<:Table[RRE],MT]( target:Query[RRT,RRE], joinType:JoinType = JoinType.Inner )(via:((LT,RT)) => MT)( implicit joinCondition : JoinCondition[MT,RRT] )
      = extendAllQueries(q).autoJoinVia2(target,joinType)(via,x=>x)
  }
}
