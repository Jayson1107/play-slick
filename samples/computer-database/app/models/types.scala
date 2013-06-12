package models.types

import slick.lifted.{MappedTypeMapper} 
//,TypeMapper,ColumnOption,Index,Shape,ForeignKeyQuery,ForeignKeyAction,Join, MappedTypeMapper, MappedProjection, Projection}


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
