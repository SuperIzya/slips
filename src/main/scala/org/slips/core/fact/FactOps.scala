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
import org.slips.core.fact.Fact.*
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
  def facts(f: Fact.Val[T]): Set[Fact[?]]

  def splitToFacts(f: Fact[T]): Fact.Val[T]

}

object FactOps {
  type TupleSignature     = List[Signature]
  private type TupleFactF = [f] => Int => FactOps[f] ?=> Fact[f]

  type ScalarFact[x] = Fact.Val[x] =:= Fact[x]

  private def getElement[T <: NonEmptyTuple, Q](index: Int)(v: T): Q = v.productElement(index).asInstanceOf[Q]

  given emptyTupleOps: FactOps[EmptyTuple] with {
    override def empty: EmptyTuple                            = EmptyTuple
    override def facts(f: Fact.Val[EmptyTuple]): Set[Fact[?]] = Set.empty

    override def splitToFacts(f: Fact[EmptyTuple]): Fact.Val[EmptyTuple] = EmptyTuple

    override def sourceFacts(f: Fact.Val[EmptyTuple]): Set[Source] = Set.empty

    override def extract(r: Fact.Val[EmptyTuple]): TupleSignature = List.empty

    override def predecessors(f: Fact.Val[EmptyTuple]): Predecessors = List.empty

    override def sources(f: Fact.Val[EmptyTuple]): Set[Signature] = Set.empty

    override def signature: Signature = Signature.Manual("")

    override def size: Int = 0

  }

  given genLastTupleOpsStep[H](using
    H: FactOps[H],
    evH: ScalarFact[H],
    ev: Fact.Val[H *: EmptyTuple] =:= Fact[H] *: EmptyTuple
  ): FactOps[H *: EmptyTuple] with {
    override def size: Int = 1

    override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

    override def sourceFacts(f: Fact.Val[H *: EmptyTuple]): Set[Fact.Source] = {
      H.sourceFacts(evH.flip(ev(f)._1))
    }

    override def extract(f: Fact.Val[H *: EmptyTuple]): TupleSignature = {
      List(ev(f)._1.signature)
    }

    override def predecessors(f: Fact.Val[H *: EmptyTuple]): Predecessors = {
      ev(f)._1.predecessors
    }

    override def sources(f: Fact.Val[H *: EmptyTuple]): Set[Signature] = {
      ev(f)._1.sources
    }

    override def facts(f: Fact.Val[H *: EmptyTuple]): Set[Fact[?]] = {
      H.facts(evH.flip(ev(f)._1))
    }

    override def splitToFacts(f: Fact[H *: EmptyTuple]): Fact.Val[H *: EmptyTuple] = {
      import org.slips.syntax.*
      val head: Fact[H]                    = f.value(_._1)
      val tuple: Fact.Val[H *: EmptyTuple] = ev.flip(head *: EmptyTuple)
      tuple
    }
  }

  given genFactOpsSingle[T: NotTuple](using
    ev: ScalarFact[T],
    T: Empty[T]
  ): FactOps[T] with {

    override def size: Int = 1

    override def empty: T = T.empty

    override def sourceFacts(f: Fact.Val[T]): Set[Fact.Source] = ev(f).alphaSources

    override def extract(r: Fact.Val[T]): TupleSignature = List(ev(r).signature)

    def predecessors(f: Fact.Val[T]): Predecessors = ev(f).predecessors

    def sources(f: Fact.Val[T]): Set[Signature] = ev(f).sources

    override def facts(f: Fact.Val[T]): Set[Fact[?]] = Set(ev(f))

    def splitToFacts(f: Fact[T]): Fact.Val[T] = ev.flip(f)
  }

  given genTupleOpsStep[H, T <: NonEmptyTuple](using
    H: FactOps[H],
    evH: ScalarFact[H],
    T: FactOps[T],
    ev: Fact.Val[H *: T] =:= Fact[H] *: Fact.TMap[T],
    evT: Fact.Val[T] =:= Fact.TMap[T],
    evM: Fact[H] *: Fact.TMap[T] =:= Fact.Val[H *: T]
  ): FactOps[H *: T] with {

    override def size: Int = T.size + 1

    override def empty: H *: T = H.empty *: T.empty

    private inline def split(f: Fact.Val[H *: T]): (Fact[H], Fact.Val[T]) = {
      val (head: Fact[H]) *: (tail: Fact.TMap[T]) = ev(f): @unchecked
      head -> evT.flip(tail)
    }

    override def sourceFacts(f: Fact.Val[H *: T]): Set[Fact.Source] = {
      val (head: Fact[H], tail: Fact.Val[T]) = split(f)
      H.sourceFacts(evH.flip(head)) ++ T.sourceFacts(tail)
    }

    override def extract(f: Fact.Val[H *: T]): TupleSignature = {
      val (head: Fact[H], tail: Fact.Val[T]) = split(f)
      head.signature +: T.extract(tail)
    }

    override def predecessors(f: Fact.Val[H *: T]): Predecessors = {
      val (head: Fact[H], tail: Fact.Val[T]) = split(f)
      head.predecessors ++ T.predecessors(tail)
    }

    override def sources(f: Fact.Val[H *: T]): Set[Signature] = {
      val (head: Fact[H], tail: Fact.Val[T]) = split(f)
      head.sources ++ T.sources(tail)
    }

    override def facts(f: Fact.Val[H *: T]): Set[Fact[?]] = {
      val (head: Fact[H], tail: Fact.Val[T]) = split(f)
      T.facts(tail) + head
    }

    override def splitToFacts(f: Fact[H *: T]): Fact.Val[H *: T] = {
      import org.slips.syntax.*
      val head: Fact[H]           = f.value(_.head)
      val tail: Fact.TMap[T]      = evT(T.splitToFacts(f.value(_.tail)))
      val tuple: Fact.Val[H *: T] = evM(head *: tail)
      tuple
    }
  }
  /*
  given unitFactOps(using ev: ScalarFact[Unit]): FactOps[Unit] with {
    override val signature: Signature = Signature.Manual("Unit")

    override def size: Int = 0

    def empty: Unit = ()

    def toVal(f: Fact[Unit]): Fact.Val[Unit] = ev.flip(f)

    def extract(r: Fact.Val[Unit]): FactOps.TupleSignature = List(signature)

    def predecessors(f: Fact.Val[Unit]): Predecessors = ev(f).predecessors

    def sources(f: Fact.Val[Unit]): Set[Signature] = ev(f).sources

    def sourceFacts(f: Fact.Val[Unit]): Set[Fact.Source] = ev(f).alphaSources

    def facts(f: Fact.Val[Unit]): Set[Fact[?]] = Set(ev(f))

    def splitToFacts(f: Fact[Unit]): Fact.Val[Unit] = ev.flip(f)
  }*/
}
