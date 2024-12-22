package org.slips.core.fact

import org.slips.{NotTuple, Signature, core}
import org.slips.core.WithSignature
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.*
import org.slips.core.macros.Macros

import scala.util.NotGiven

sealed trait Fact[T <: Any : NotTuple](using T: FactOps[T], F: Signature.SignType[Fact[T]]) extends WithSignature {
  self =>

  type Src
  val sample: T

  def source: Fact.Source[Src]

  def signature: Signature =
    F.signature.unite(T.signature)((f, t) => s"$f[$t]($sample)")
}

object Fact {

  type TMap        = [x <: NonEmptyTuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: NonEmptyTuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped   = [x <: NonEmptyTuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match {
    case a *: EmptyTuple => Fact[a] *: EmptyTuple
    case a *: t => Fact[a] *: TMap[t]
    case _ => Fact[X]
  }

  type InverseVal[X] = X match {
    case Fact[h] *: EmptyTuple.type => h *: EmptyTuple
    case Fact[h] *: t => h *: InverseVal[t]
    case Fact[a] => a
  }

  val unit: Fact[Unit] = Literal.Unit

  sealed trait CanBeLiteral[T]

  sealed class Literal[I : FactOps : ScalarFact] private[slips] (override val sample: I)
      extends Fact.Source[I](Signature.Manual(sample.toString), sample, None) {
    type Src = I
  }

  sealed class Source[T] private[Fact] (
    override val signature: Signature,
    override val sample: T,
    val sourceCondition: Option[Condition.Source[T]]
  )(using val scalarEv: ScalarFact[T], val factOps: FactOps[T]) extends Fact[T] {
    self =>
    override type Src = T
    override val source: Source[T] = self
  }

  final case class Map[T, Q: FactOps](pred: Fact[T], map: T => Q, mapSign: Signature) extends Fact[Q] {
    override type Src = pred.Src
    override val source: Source[Src] = pred.source

    override val signature: Signature = Signature.DerivedBinary(pred.signature, mapSign, (s1, s2) => s"$s1 -> $s2")
    override val sample: Q            = map(pred.sample)
  }

  object Literal {
    case object Unit extends Literal[Unit](())
  }

  object Source {
    def apply[T](source: Condition.Source[T])(using T: FactOps[T], F: ScalarFact[T]): Source[T] =
      new Source(source.signature, T.empty, Some(source))
  }

  object CanBeLiteral {
    given [T <: NonEmptyTuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}

    given [T](using NotTuple[T], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}
  }

  extension [T](f: Fact.Val[T]) {
    private[slips] def sourceConditions(using T: FactOps[T]): Set[Condition.Source[?]] = T.sourceConditions(f)
    private[slips] def signature(using T: FactOps[T]): Signature                       = T.extract(f)
    private[slips] def sources(using T: FactOps[T]): Set[Fact.Source[?]]               = T.sources(f)
    private[slips] def facts(using T: FactOps[T]): List[Fact[?]]                       = T.facts(f)
  }
}
