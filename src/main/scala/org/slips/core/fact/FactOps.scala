package org.slips.core.fact

import cats.Monoid
import org.slips.Environment
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core
import org.slips.core.Empty
import org.slips.core.Macros
import org.slips.core.SignatureStrategy
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.Fact.Predecessors
import org.slips.core.fact.Fact.Source
import org.slips.core.fact.Fact.TMap
import org.slips.core.fact.Fact.Val
import scala.annotation.tailrec
import scala.compiletime.error
import scala.deriving.Mirror
import scala.util.NotGiven

sealed trait FactOps[T] {
  def signature: Signature = Signature.Manual(Macros.signType[T])

  def size: Int

  def empty: T

  def extract(r: Fact.Val[T]): FactOps.TupleSignature
  def predecessors(f: Fact.Val[T]): Predecessors
  def sources(f: Fact.Val[T]): Set[Signature]
  def sourceFacts(f: Fact.Val[T]): Set[Fact.Source]
  def facts(f: Fact.Val[T]): Set[Fact[_]]

  def splitToFacts(f: Fact[T]): Fact.Val[T]

}

object FactOps extends FactOps.LowPrio {
  type TupleSignature     = List[Signature]
  private type TupleFactF = [f] => Int => FactOps[f] ?=> Fact[f]

  given typeFromTuple[T <: NonEmptyTuple](using T: TupleOps[T]): FactOps[T] = T

  private def getElement[T <: NonEmptyTuple, Q](index: Int)(v: T): Q = v.productElement(index).asInstanceOf[Q]

  sealed trait TupleOps[T <: Tuple] extends FactOps[T] {

    override val size: Int = index
    val index: Int

  }

  trait LowPrio {

    given unitFactOps(using ev: Fact.Val[Unit] =:= Fact[Unit]): FactOps[Unit] with {
      override val signature: Signature = Signature.Manual("Unit")

      def size: Int = 0

      def empty: Unit = ()

      def toVal(f: Fact[Unit]): Fact.Val[Unit] = f

      def extract(r: Fact.Val[Unit]): FactOps.TupleSignature = List(signature)

      def predecessors(f: Fact.Val[Unit]): Predecessors = List.empty

      def sources(f: Fact.Val[Unit]): Set[Signature] = Set.empty

      def sourceFacts(f: Fact.Val[Unit]): Set[Fact.Source] = Set.empty

      def facts(f: Fact.Val[Unit]): Set[Fact[_]] = Set(Fact.unit)

      def splitToFacts(f: Fact[Unit]): Fact.Val[Unit] = ev.flip(Fact.unit)
    }
  }

  object TupleOps extends TupleOps.LowPrio {

    trait LowPrio {
      given TupleOps[EmptyTuple] with {
        override val index: Int = 0
        override val size: Int  = 0

        override def facts(f: EmptyTuple): Set[Fact[_]] = Set.empty

        override def splitToFacts(f: Fact[EmptyTuple]): EmptyTuple = EmptyTuple

        override def sourceFacts(f: EmptyTuple): Set[Source] = Set.empty

        override def extract(r: EmptyTuple): TupleSignature = List.empty

        override def empty: EmptyTuple = EmptyTuple

        override def predecessors(f: EmptyTuple): Predecessors = List.empty

        override def sources(f: EmptyTuple): Set[Signature] = Set.empty

      }

      given genTupleOpsStart[H](using
        H: FactOps[H],
        ev: Fact[H] *: EmptyTuple =:= Fact.Val[H *: EmptyTuple]
      ): TupleOps[H *: EmptyTuple] with {
        override val index: Int = 1

        override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

        override def sourceFacts(f: Fact.Val[H *: EmptyTuple]): Set[Fact.Source] = {
          val (head: Fact[H]) *: EmptyTuple = f: @unchecked

          head.alphaSources
        }

        override def facts(f: Val[H *: EmptyTuple]): Set[Fact[_]] = {
          val (head: Fact[H]) *: EmptyTuple = f: @unchecked
          Set(head)
        }

        override def extract(f: Fact.Val[H *: EmptyTuple]): TupleSignature = {
          val (head: Fact[H]) *: EmptyTuple = f: @unchecked
          List(head.signature)
        }

        def predecessors(f: Fact.Val[H *: EmptyTuple]): Predecessors = {
          val (head: Fact[H]) *: EmptyTuple = f: @unchecked
          head.predecessors
        }

        def sources(f: Fact.Val[H *: EmptyTuple]): Set[Signature] = {
          val (head: Fact[H]) *: EmptyTuple = f: @unchecked
          head.sources
        }

        override def splitToFacts(f: Fact[H *: EmptyTuple]): Fact.Val[H *: EmptyTuple] = {
          import org.slips.syntax.*
          ev(f.value(_.head) *: EmptyTuple)
        }
      }
    }

    given genTupleOpsStep[H, T <: Tuple](using
      H: FactOps[H],
      evH: Fact.Val[H] =:= Fact[H],
      T: TupleOps[T],
      evT: Fact.Val[T] =:= Fact.TMap[T],
      ev: Fact.Val[H *: T] =:= Fact[H] *: Fact.TMap[T]
    ): TupleOps[H *: T] with {
      override val index: Int = T.index + 1

      override def empty: H *: T = H.empty *: T.empty

      override def sourceFacts(f: Fact.Val[H *: T]): Set[Fact.Source] = {
        val (head: Fact[H], tail: Fact.TMap[T]) = split(f)
        H.sourceFacts(evH.flip(head)) ++ T.sourceFacts(evT.flip(tail))
      }

      private inline def split(f: Fact.Val[H *: T]): (Fact[H], Fact.TMap[T]) = {
        val (head: Fact[H]) *: (tail: Fact.TMap[T]) = ev(f): @unchecked
        head -> tail
      }

      override def extract(f: Fact.Val[H *: T]): TupleSignature = {
        val (head: Fact[H], tail: Fact.TMap[T]) = split(f)
        head.signature +: T.extract(evT.flip(tail))
      }

      override def predecessors(f: Fact.Val[H *: T]): Predecessors = {
        val (head: Fact[H], tail: Fact.TMap[T]) = split(f)
        head.predecessors ++ T.predecessors(evT.flip(tail))
      }
      override def sources(f: Fact.Val[H *: T]): Set[Signature]    = {
        val (head: Fact[H], tail: Fact.TMap[T]) = split(f)
        head.sources ++ T.sources(evT.flip(tail))
      }

      override def facts(f: Fact.Val[H *: T]): Set[Fact[_]] = {
        val (head: Fact[H], tail: Fact.TMap[T]) = split(f)
        T.facts(evT.flip(tail)) + head
      }

      override def splitToFacts(f: Fact[H *: T]): Fact.Val[H *: T] = {
        import org.slips.syntax.*
        val head: Fact[H]                  = f.value(_.head)
        val tail: Fact.Val[T]              = T.splitToFacts(f.value(_.tail))
        val tuple: Fact[H] *: Fact.TMap[T] = head *: evT(tail)
        ev.flip(tuple)
      }
    }
  }

  given genFactOpsSingle[T: NotTuple](using ev: Fact.Val[T] =:= Fact[T], T: Empty[T]): FactOps[T] with {

    override def size: Int = 1

    override def empty: T = T.empty

    override def sourceFacts(f: Fact.Val[T]): Set[Fact.Source] = ev(f).alphaSources

    override def extract(r: Fact.Val[T]): TupleSignature = List(ev(r).signature)

    def predecessors(f: Fact.Val[T]): Predecessors = ev(f).predecessors

    def sources(f: Fact.Val[T]): Set[Signature] = ev(f).sources

    override def facts(f: Fact.Val[T]): Set[Fact[_]] = Set(ev(f))

    def splitToFacts(f: Fact[T]): Fact.Val[T] = ev.flip(f)
  }

}
