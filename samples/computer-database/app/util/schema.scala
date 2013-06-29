package util
package object schema{
  import play.api.db.slick.driver.simple._
  import slick.lifted.{Projection,ColumnBase}
  
  trait TypedId extends Any{
    val untypedId : Long
  }
  trait HasUntypedId{
    //this:Table[_]=>
    def untypedId : Column[Long]
  }
  trait HasId{
    //this:Table[_]=>
    type IdType <: TypedId
    def id : Column[IdType]
  }
  trait Mapping[E]{
    type Columns <: Product
    def create : Columns => E
    def extract   : E => Option[Columns]
  }
  def Mapping[E,C <:Product]( to: C => E )( from: E => Option[C] ) = new Mapping[E]{
    type Columns = C
    def create = to
    def extract = from
  }
  object NameGenerator{
    var fk_inc = 0
    var idx_inc = 0
    def fkName(name:String="")  = s"fk_${name}_" +{fk_inc += 1;fk_inc}
    def fkName : String = fkName()
    def idxName(name:String="") = s"idx_${name}_"+{idx_inc += 1;idx_inc}
    def idxName : String = idxName()
  }

  trait MappingHelpers[E]{
    val mapping : Mapping[E]
    import mapping._
    implicit def mappingHelpers2  [T <: Product]( p:Projection[T] ) = new{
      def mapInsert( from: Columns => T ) = mapWith[E](_ => ???, (e:E) => extract(e).map(from))
      def mapOption( to:   T => Option[E] ) = mapWith[Option[E]] (to, _ => ???)
      def mapWith[E]( to: T => E, from: E => Option[T] ) = p <> (to,from)
    }
  }
  trait StarProjection[E] extends MappingHelpers[E]{
    import mapping._
    def columns : Projection[Columns]
    def * = columns mapWith (create,extract)
  }
  trait OptionMapping[E] extends Table[E]{
    def ? : ColumnBase[Option[E]]      
  }
  trait AutoIncTyped[E] extends AutoInc[E] with HasId{
    def autoIncTypedId = autoIncCount returning id
  }
  trait AutoInc[E] extends Table[E] with HasUntypedId{
    def autoIncCount : ColumnBase[E]
    def autoIncUntypedId = autoIncCount returning untypedId
  }
  /*
    implicit def mappingHelpers  [T <: Product]( p:Projection[T] ) = new{
      def mapInsert[R]( from: R => T ) = mapWith[T,R] (p) (_ => ???) (from)
      def mapOption[R]( to:   T => R ) = mapWith[T,R] (p) (to)   (_ => ???)
    }
    def mapWith[T <: Product,R]( p:Projection[T] )( to: T => R )( from: R => T ) = p <> (to,(x:R) => Some(from(x)))
  */
}
