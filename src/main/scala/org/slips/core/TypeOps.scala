package org.slips.core

import cats.Monoid
import org.slips.core.Fact.*
import org.slips.core.conditions.Condition
import scala.annotation.tailrec
import scala.compiletime.error
import scala.compiletime.ops.int.S
import scala.deriving.Mirror
import scala.util.NotGiven
import shapeless3.deriving.*

sealed trait TypeOps[T](using TypeOps.Size[T]) {
  val signature: String = Macros.signType[T]

  def empty: T
  def forSource(src: Condition[T]): Fact.Val[T]
  def toVal(f: Fact[T]): Val[T]
  def extract(r: Fact.Val[T]): TypeOps.TupleSignature
}

object TypeOps {
  sealed trait Size[T]:
    val size: Int

  object Size {

    given empty: Size[EmptyTuple] with {
      override val size: Int = 0
    }

    given tuple[H, T <: Tuple](
      using T: Size[T]
    ): Size[H *: T] with {
      override val size: Int = 1 + T.size
    }

    given scalar[T](
      using NotGiven[T <:< Tuple]
    ): Size[T] with {
      override val size: Int = 1
    }
  }

  type TupleSignature = List[String]

  private def getElement[T <: NonEmptyTuple, Q](index: Int)(v: T): Q = v.productElement(index).asInstanceOf[Q]

  trait Empty[T]:
    def empty: T

  object Empty {
    given Empty[Unit] with   { override def empty: Unit = ()   }
    given Empty[Char] with   { override def empty: Char = 0    }
    given Empty[Byte] with   { override def empty: Byte = 0    }
    given Empty[Short] with  { override def empty: Short = 0   }
    given Empty[Int] with    { override def empty: Int = 0     }
    given Empty[Long] with   { override def empty: Long = 0L   }
    given Empty[Float] with  { override def empty: Float = 0f  }
    given Empty[Double] with { override def empty: Double = 0  }
    given Empty[String] with { override def empty: String = "" }

    given genEmptyStart[T](
      using T: Empty[T]
    ): Empty[T *: EmptyTuple] with {
      override def empty: T *: EmptyTuple = T.empty *: EmptyTuple
    }

    given genEmptyStep[T <: NonEmptyTuple, H](
      using H: Empty[H],
      T: Empty[T]
    ): Empty[H *: T] with {
      override def empty: H *: T = H.empty *: T.empty
    }

    given genEmptyByMonoid[T](
      using T: Monoid[T]
    ): Empty[T] with {
      override def empty: T = T.empty
    }

    given genEmptyCaseClass[T](
      using inst: K0.ProductInstances[Empty, T]
    ): Empty[T] with {
      override def empty: T = inst.construct([t] ⇒ (e: Empty[t]) ⇒ e.empty)
    }

    inline def derived[T](using gen: K0.ProductGeneric[T]): Empty[T] =
      genEmptyCaseClass[T]
  }

  given genTypeOpsSingle[T: Size](
    using ev: Fact[T] =:= Fact.Val[T],
    T: Empty[T],
    ev3: NotGiven[T <:< Tuple]
  ): TypeOps[T] with {
    override def empty: T                                  = T.empty
    override def forSource(src: Condition[T]): Fact.Val[T] = Dummy(src, empty)
    override def toVal(f: Fact[T]): Val[T]                 = f
    override def extract(r: Fact.Val[T]): TupleSignature   = List(ev.flip(r).signature)
  }

  private type TupleFactF = [x] ⇒ Int ⇒ TypeOps[x] ?=> Fact[x]

  sealed trait TupleOps[T <: NonEmptyTuple](using size: Size[T])
      extends TypeOps[T] {

    val index: Int

    def chainT[Q <: NonEmptyTuple](f: TupleFactF)(using size: Size[Q]): Fact.TMap[T]

    def forSource(src: Condition[T]): Fact.Val[T] = chainT {
      [H] ⇒ (index: Int) ⇒ (H: TypeOps[H]) ?=> FromTuple[T, H](src, index, H.empty)
    }

    def toVal(f: Fact[T]): Val[T] = {
      chainT {
        [x] ⇒
          (index: Int) ⇒
            (to: TypeOps[x]) ?=>
              fromFactTuple[T, x](f, getElement[T, x](index)(_), index)(
                using to,
                size
            )
      }
    }
  }

  object TupleOps {
    given genTupleOpsStart[H](
      using H: TypeOps[H],
      S: Size[H *: EmptyTuple]
    ): TupleOps[H *: EmptyTuple] with {
      override val index: Int = 1

      override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      override def chainT[Q <: NonEmptyTuple](f: TupleFactF)(using Q: Size[Q]): Fact.TMap[H *: EmptyTuple] = {
        f[H](Q.size - index) *: EmptyTuple
      }

      override def extract(r: Fact.Val[H *: EmptyTuple]): TupleSignature = List(
        r.head.signature
      )
    }

    given genTupleOpsStep[H, T <: NonEmptyTuple](
      using H: TypeOps[H],
      T: TupleOps[T],
      S: Size[H *: T],
      ev: Fact.Val[H *: T] <:< NonEmptyTuple
    ): TupleOps[H *: T] with {
      override val index: Int = T.index + 1

      override def empty: H *: T = H.empty *: T.empty

      override def chainT[Q <: NonEmptyTuple](f: TupleFactF)(using Q: Size[Q]): Fact.TMap[H *: T] = {
        val prev: Fact.TMap[T] = T.chainT[Q](f)
        f[H](Q.size - index) *: prev
      }

      override def extract(r: Fact.Val[H *: T]): TupleSignature = {
        val head *: tail = r
        head.signature +: T.extract(tail)
      }
    }
  }

  given typeFromTuple[T <: NonEmptyTuple](
    using T: TupleOps[T]
  ): TypeOps[T] = T

}
