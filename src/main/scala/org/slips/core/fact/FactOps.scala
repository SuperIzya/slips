package org.slips.core.fact

import cats.Monoid
import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.Empty
import org.slips.core.Macros
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import scala.annotation.tailrec
import scala.compiletime.error
import scala.deriving.Mirror
import scala.util.NotGiven

sealed trait FactOps[T] {
  val signature: String = Macros.signType[T]

  def size: Int

  def empty: T
  def toVal(
    f: Fact[T]
  ): Fact.Val[T]

  def extract(
    r: Fact.Val[T]
  ): FactOps.TupleSignature
  def predecessors(
    f: Fact.Val[T]
  ): Set[Fact[_]]
  def sources(
    f: Fact.Val[T]
  ): Set[Condition.Source[_]]
  def sourceFacts(
    f: Fact.Val[T]
  ): Set[Fact.Source[_]]

  def allCondition: Condition[T]

}

object FactOps {
  type TupleSignature     = List[String]
  private type TupleFactF = [x] => Int => FactOps[x] ?=> Fact[x]

  given typeFromTuple[T <: NonEmptyTuple](using T: TupleOps[T]): FactOps[T] = T

  private def getElement[T <: NonEmptyTuple, Q](
    index: Int
  )(
    v: T
  ): Q = v.productElement(index).asInstanceOf[Q]

  sealed trait TupleOps[T <: NonEmptyTuple]
      extends FactOps[T] {

    val index: Int

    override val size: Int = index

    def chainT[Q <: NonEmptyTuple](
      f: TupleFactF
    ): Fact.TMap[T]

    override def toVal(
      f: Fact[T]
    ): Fact.Val[T] = chainT {
      [x] =>
        (index: Int) =>
          (to: FactOps[x]) ?=>
            Fact.fromFactTuple[T, x](f, getElement[T, x](index)(_), index)(
              using to
          )
    }
  }

  object TupleOps {
    given genTupleOpsStart[H](
      using H: FactOps[H],
      ev: Fact.Val[H] =:= Fact[H]
    ): TupleOps[H *: EmptyTuple] with {
      override val index: Int = 1

      override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      override def sourceFacts(
        f: Fact.Val[H *: EmptyTuple]
      ): Set[Fact.Source[_]] = f.head.sourceFacts

      override def chainT[Q <: NonEmptyTuple](
        f: TupleFactF
      ): Fact.TMap[H *: EmptyTuple] = {
        f[H](0) *: EmptyTuple
      }

      override def extract(
        r: Fact.Val[H *: EmptyTuple]
      ): TupleSignature = List(r.head.signature)

      def predecessors(
        f: Fact.Val[H *: EmptyTuple]
      ): Set[Fact[_]] = {
        val head *: EmptyTuple = f
        head.predecessors
      }
      def sources(
        f: Fact.Val[H *: EmptyTuple]
      ): Set[Condition.Source[_]] = {
        val head *: EmptyTuple = f
        head.sources
      }

      override def allCondition: Condition[H *: EmptyTuple] = for {
        h <- Condition.all[H]
      } yield ev(h) *: EmptyTuple
    }

    given genTupleOpsStep[H, T <: NonEmptyTuple](
      using H: FactOps[H],
      evH: Fact.Val[H] =:= Fact[H],
      T: TupleOps[T],
      ev: Fact.Val[H *: T] <:< NonEmptyTuple,
      evT: Fact.Val[T] =:= Fact.TMap[T],
      evR: Fact.ReverseVal[Fact[H] *: Fact.Val[T]] =:= H *: T
    ): TupleOps[H *: T] with {
      override val index: Int = T.index + 1

      override def empty: H *: T = H.empty *: T.empty

      override def sourceFacts(
        f: Fact.Val[H *: T]
      ): Set[Fact.Source[_]] = {
        val head *: tail = f
        H.sourceFacts(head.toVal) ++ T.sourceFacts(tail)
      }

      override def chainT[Q <: NonEmptyTuple](
        f: TupleFactF
      ): Fact.TMap[H *: T] = {
        val prev: Fact.TMap[T] = T.chainT[Q](f)
        f[H](index) *: prev
      }

      override def extract(
        r: Fact.Val[H *: T]
      ): TupleSignature = {
        val head *: tail = r
        head.signature +: T.extract(tail)
      }

      override def predecessors(
        f: Fact.Val[H *: T]
      ): Set[Fact[_]] = {
        val head *: tail = f
        head.predecessors ++ T.predecessors(tail)
      }
      override def sources(
        f: Fact.Val[H *: T]
      ): Set[Condition.Source[_]] = {
        val head *: tail = f
        head.sources ++ T.sources(tail)
      }

      override def allCondition: Condition[H *: T] = for {
        h <- Condition.all[H]
        t <- T.allCondition
      } yield evH(h) *: t

    }
  }

  given genFactOpsSingle[T](
    using ev: Fact.Val[T] =:= Fact[T],
    T: Empty[T]
  ): FactOps[T] with {

    override def size: Int = 1

    override def empty: T = T.empty

    override def sourceFacts(
      f: Fact.Val[T]
    ): Set[Fact.Source[_]] = ev(f).sourceFacts

    override def toVal(
      f: Fact[T]
    ): Fact.Val[T] = ev.flip(f)
    override def extract(
      r: Fact.Val[T]
    ): TupleSignature = List(ev(r).signature)
    def predecessors(
      f: Fact.Val[T]
    ): Set[Fact[_]] = ev(f).predecessors
    def sources(
      f: Fact.Val[T]
    ): Set[Condition.Source[_]] = ev(f).sources

    override def allCondition: Condition[T] = Condition.all[T]

  }

}
