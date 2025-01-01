package org.slips.core

import cats.Monoid
import magnolia1.*
import org.slips.core
import scala.compiletime.summonAll
import scala.deriving.Mirror

trait Empty[T] {
  def empty: T
}

object Empty extends AutoDerivation[Empty] {
  given Empty[Unit]   { override def empty: Unit = ()   }
  given Empty[Char]   { override def empty: Char = 0    }
  given Empty[Byte]   { override def empty: Byte = 0    }
  given Empty[Short]  { override def empty: Short = 0   }
  given Empty[Int]     { override def empty: Int = 0     }
  given Empty[Long]    { override def empty: Long = 0L   }
  given Empty[Float]   { override def empty: Float = 0f  }
  given Empty[Double]  { override def empty: Double = 0  }
  given Empty[String]  { override def empty: String = "" }

  given genOption: [T] => Empty[Option[T]]  {
    override def empty: Option[T] = None
  }

  given genEmptyStart: [T: {Empty as T}] => Empty[T *: EmptyTuple]  {
    override def empty: T *: EmptyTuple = T.empty *: EmptyTuple
  }

  given genEmptyStep: [T <: NonEmptyTuple: {Empty as T}, H: {Empty as H}] => Empty[H *: T]  {
    override def empty: H *: T = H.empty *: T.empty
  }

  given genEmptyByMonoid: [T: {Monoid as T}] => Empty[T]  {
    override def empty: T = T.empty
  }

  given genEmptyCaseClass: [T] => (inst: Mirror.ProductOf[T]) => (T: Empty[inst.MirroredElemTypes]) => Empty[T] =
    new Empty[T] {
      override def empty: T = inst.fromTuple(T.empty)
    }

  override def join[T](
    ctx: CaseClass[Empty, T]
  ): Empty[T] = new Empty[T] {
    override def empty: T = ctx.rawConstruct(ctx.params.map { p => p.typeclass.empty })
  }

  override def split[T](
    ctx: SealedTrait[Empty, T]
  ): Empty[T] = new Empty[T] {
    override def empty: T = ctx.subtypes.head.typeclass.empty
  }
}
