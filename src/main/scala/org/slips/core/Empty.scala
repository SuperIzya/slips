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
  given Empty[Unit] with   { override def empty: Unit = ()   }
  given Empty[Char] with   { override def empty: Char = 0    }
  given Empty[Byte] with   { override def empty: Byte = 0    }
  given Empty[Short] with  { override def empty: Short = 0   }
  given Empty[Int] with    { override def empty: Int = 0     }
  given Empty[Long] with   { override def empty: Long = 0L   }
  given Empty[Float] with  { override def empty: Float = 0f  }
  given Empty[Double] with { override def empty: Double = 0  }
  given Empty[String] with { override def empty: String = "" }

  given genOption[T]: Empty[Option[T]] with {
    override def empty: Option[T] = None
  }

  given genEmptyStart[T](using T: Empty[T]): Empty[T *: EmptyTuple] with {
    override def empty: T *: EmptyTuple = T.empty *: EmptyTuple
  }

  given genEmptyStep[T <: NonEmptyTuple, H](using H: Empty[H], T: Empty[T]): Empty[H *: T] with {
    override def empty: H *: T = H.empty *: T.empty
  }

  given genEmptyByMonoid[T](using T: Monoid[T]): Empty[T] with {
    override def empty: T = T.empty
  }

  given genEmptyCaseClass[T](using inst: Mirror.ProductOf[T])(using T: Empty[inst.MirroredElemTypes]): Empty[T] =
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
