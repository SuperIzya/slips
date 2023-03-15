package org.slips.core.fact

import cats.Monoid
import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.Empty
import org.slips.core.Macros
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.Fact.Predecessors
import org.slips.core.fact.Fact.TMap
import scala.annotation.tailrec
import scala.compiletime.error
import scala.deriving.Mirror
import scala.util.NotGiven

sealed trait FactOps[T] {
  val signature: String = Macros.signType[T]

  def size: Int

  def empty: T
  def toVal(f: Fact[T]): Fact.Val[T]

  def extract(r: Fact.Val[T]): FactOps.TupleSignature
  def predecessors(f: Fact.Val[T]): Predecessors
  def sources(f: Fact.Val[T]): Set[Condition.Source[_]]
  def sourceFacts(f: Fact.Val[T]): Set[Fact.Source]

}

object FactOps {
  type TupleSignature     = List[String]
  private type TupleFactF = [f] => Int => FactOps[f] ?=> Fact[f]

  given typeFromTuple[T <: NonEmptyTuple](using T: TupleOps[T]): FactOps[T] = T

  private def getElement[T <: NonEmptyTuple, Q](index: Int)(v: T): Q = v.productElement(index).asInstanceOf[Q]

  sealed trait TupleOps[T <: NonEmptyTuple] extends FactOps[T] {

    val index: Int

    override val size: Int = index

    def chainT(f: TupleFactF): Fact.TMap[T]

    def toVal(f: TMap[T]): Fact.Val[T]
  }

  object TupleOps {
    given genTupleOpsStart[H](using H: FactOps[H], ev: Fact.Val[H] =:= Fact[H]): TupleOps[H *: EmptyTuple] with {
      override val index: Int = 1

      override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      override def sourceFacts(f: Fact.Val[H *: EmptyTuple]): Set[Fact.Source] = {
        val (head: Fact[H]) *: EmptyTuple = f

        head.sourceFacts
      }

      override def chainT(f: TupleFactF): Fact.TMap[H *: EmptyTuple] = {
        f[H](0) *: EmptyTuple
      }

      override def extract(f: Fact.Val[H *: EmptyTuple]): TupleSignature = {
        val (head: Fact[H]) *: EmptyTuple = f
        List(head.signature)
      }

      def predecessors(f: Fact.Val[H *: EmptyTuple]): Predecessors = {
        val (head: Fact[H]) *: EmptyTuple = f
        head.predecessors
      }

      def sources(f: Fact.Val[H *: EmptyTuple]): Set[Condition.Source[_]] = {
        val (head: Fact[H]) *: EmptyTuple = f
        head.sources
      }
    }

    given genTupleOpsStep[H, T <: NonEmptyTuple](using
      H: FactOps[H],
      evH: Fact.Val[H] =:= Fact[H],
      evHS: NotTuple[H],
      T: TupleOps[T],
      evT: Fact.Val[T] =:= TMap[T],
      ev: Fact.Val[H *: T] =:= Fact[H] *: TMap[T],
      ev2: Fact[H] *: TMap[T] =:= Fact.Val[H *: T]
    ): TupleOps[H *: T] with {
      override val index: Int = T.index + 1

      override def empty: H *: T = H.empty *: T.empty

      override def sourceFacts(f: Fact.Val[H *: T]): Set[Fact.Source] = {
        val head *: tail = ev.apply(f)
        H.sourceFacts(H.toVal(head)) ++ T.sourceFacts(T.toVal(tail))
      }

      override def chainT(f: TupleFactF): Fact.TMap[H *: T] = {
        val prev: Fact.TMap[T] = T.chainT(f)
        f[H](index) *: prev
      }

      override def extract(r: Fact.Val[H *: T]): TupleSignature = {
        val (head: Fact[H]) *: tail = ev.apply(r)
        head.signature +: T.extract(T.toVal(tail))
      }

      override def predecessors(f: Fact.Val[H *: T]): Predecessors        = {
        val (head: Fact[H]) *: tail = ev.apply(f)
        head.predecessors ++ tail.predecessors
      }
      override def sources(f: Fact.Val[H *: T]): Set[Condition.Source[_]] = {
        val (head: Fact[H]) *: tail = ev.apply(f)
        head.sources ++ tail.sources
      }

    }
  }

  given genFactOpsSingle[T](using ev: Fact.Val[T] =:= Fact[T], T: Empty[T]): FactOps[T] with {

    override def size: Int = 1

    override def empty: T = T.empty

    override def sourceFacts(f: Fact.Val[T]): Set[Fact.Source] = ev(f).sourceFacts

    override def toVal(f: Fact[T]): Fact.Val[T] = ev.flip(f)

    override def extract(r: Fact.Val[T]): TupleSignature = List(ev(r).signature)

    def predecessors(f: Fact.Val[T]): Predecessors = ev(f).predecessors

    def sources(f: Fact.Val[T]): Set[Condition.Source[_]] = ev(f).sources

  }

}
