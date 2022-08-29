package org.slips.core

import cats.Monoid
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.Fact._
import org.slips.core.FactSize
import scala.annotation.tailrec
import scala.compiletime.error
import scala.compiletime.ops.int.S
import scala.deriving.Mirror
import scala.util.NotGiven
import shapeless3.deriving.*

sealed trait TypeOps[T](using FactSize[T]) {
  val signature: String = Macros.signType[T]

  def empty: T
  def toVal(f: Fact[T]): Fact.Val[T]
  def extract(r: Fact.Val[T]): TypeOps.TupleSignature
  def predecessors(f: Fact.Val[T]): Set[Fact[_]]
  def sources(f: Fact.Val[T]): Set[Condition.Source[_]]
  def sourceFacts(f: Fact.Val[T]): Set[Fact.Source[_]]
}

object TypeOps {
  type TupleSignature     = List[String]
  private type TupleFactF = [x] => Int => TypeOps[x] ?=> Fact[x]

  given typeFromTuple[T <: NonEmptyTuple](using T: TupleOps[T]): TypeOps[T] = T

  private def getElement[T <: NonEmptyTuple, Q](index: Int)(v: T): Q = v.productElement(index).asInstanceOf[Q]

  sealed trait TupleOps[T <: NonEmptyTuple](using size: FactSize[T])
      extends TypeOps[T] {

    val index: Int

    def chainT[Q <: NonEmptyTuple](f: TupleFactF)(using size: FactSize[Q]): Fact.TMap[T]

    override def toVal(f: Fact[T]): Fact.Val[T] = chainT {
      [x] =>
        (index: Int) =>
          (to: TypeOps[x]) ?=>
            Fact.fromFactTuple[T, x](f, getElement[T, x](index)(_), index)(
              using to,
              size
          )
    }
  }

  object TupleOps {
    given genTupleOpsStart[H](using H: TypeOps[H], S: FactSize[H *: EmptyTuple]): TupleOps[H *: EmptyTuple] with {
      override val index: Int = 1

      override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      override def sourceFacts(f: Val[H *: EmptyTuple]): Set[Source[_]] = f.head.sourceFacts

      override def chainT[Q <: NonEmptyTuple](f: TupleFactF)(using Q: FactSize[Q]): Fact.TMap[H *: EmptyTuple] = {
        f[H](Q.size - index) *: EmptyTuple
      }

      override def extract(r: Fact.Val[H *: EmptyTuple]): TupleSignature = List(r.head.signature)

      def predecessors(f: Fact.Val[H *: EmptyTuple]): Set[Fact[_]]        = {
        val head *: EmptyTuple = f
        head.predecessors
      }
      def sources(f: Fact.Val[H *: EmptyTuple]): Set[Condition.Source[_]] = {
        val head *: EmptyTuple = f
        head.sources
      }
    }

    given genTupleOpsStep[H, T <: NonEmptyTuple](
      using H: TypeOps[H],
      evF: Fact.Val[H] =:= Fact[H],
      T: TupleOps[T],
      S: FactSize[H *: T],
      ev: Fact.Val[H *: T] <:< NonEmptyTuple
    ): TupleOps[H *: T] with {
      override val index: Int = T.index + 1

      override def empty: H *: T = H.empty *: T.empty

      override def sourceFacts(f: Fact.Val[H *: T]): Set[Source[_]]                               = {
        val (head: Fact[H]) *: tail = f
        H.sourceFacts(head.toVal) ++ T.sourceFacts(tail)
      }

      override def chainT[Q <: NonEmptyTuple](f: TupleFactF)(using Q: FactSize[Q]): Fact.TMap[H *: T] = {
        val prev: Fact.TMap[T] = T.chainT[Q](f)
        f[H](Q.size - index) *: prev
      }

      override def extract(r: Fact.Val[H *: T]): TupleSignature = {
        val head *: tail = r
        head.signature +: T.extract(tail)
      }

      override def predecessors(f: Fact.Val[H *: T]): Set[Fact[_]]        = {
        val head *: tail = f
        head.predecessors ++ T.predecessors(tail)
      }
      override def sources(f: Fact.Val[H *: T]): Set[Condition.Source[_]] = {
        val head *: tail = f
        head.sources ++ T.sources(tail)
      }

    }
  }

  given genTypeOpsSingle[T: FactSize](
    using ev: Fact.Val[T] =:= Fact[T],
    T: Empty[T],
    ev3: NotGiven[T <:< Tuple]
  ): TypeOps[T] with {

    override def empty: T = T.empty

    override def sourceFacts(f: Val[T]): Set[Source[_]] = ev(f).sourceFacts

    override def toVal(f: Fact[T]): Fact.Val[T]           = ev.flip(f)
    override def extract(r: Fact.Val[T]): TupleSignature  = List(ev(r).signature)
    def predecessors(f: Fact.Val[T]): Set[Fact[_]]        = ev(f).predecessors
    def sources(f: Fact.Val[T]): Set[Condition.Source[_]] = ev(f).sources
  }

}
