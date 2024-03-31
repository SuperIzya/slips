package org.slips.core.fact

import org.slips.NotTuple
import org.slips.Signature
import org.slips.core
import org.slips.core.WithSignature
import org.slips.core.macros.Macros
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.*
import scala.util.NotGiven

sealed trait Fact[T <: Any : NotTuple](using T: FactOps[T], F: Signature.SignType[Fact[T]]) extends WithSignature { self =>
  val sample: T

  val predecessors: List[Fact[?]]

  val sources: Set[Condition.Source[?]]
  override def signature: Signature =
    Signature.derivedBinary(F, T, (f, t) => s"$f[$t]($sample)")

  def alphaSources: Set[Fact.Source] = predecessors.collect { case x: Fact.Source => x }.toSet

  val isAlpha: Boolean = false

}

object Fact {

  type Source = Fact.Alpha.Source[?]

  type TMap        = [x <: NonEmptyTuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: NonEmptyTuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped   = [x <: NonEmptyTuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match
    case a *: EmptyTuple.type => Fact[a] *: EmptyTuple
    case a *: t               => Fact[a] *: Val[t]
    case _                    => Fact[X]

  type InverseVal[X] = X match
    case Fact[h] *: EmptyTuple.type => h *: EmptyTuple
    case Fact[h] *: t               => h *: InverseVal[t]
    case Fact[a]                    => a

  val unit: Fact[Unit] = Literal.Unit

  sealed trait CanBeLiteral[T]

  final case class Map[T, Q: FactOps](
    override val signature: Signature,
    f: T => Q,
    rep: Fact[T]
  ) extends Fact[Q] { self =>
    override val sample: Q                   = f(rep.sample)
    override val predecessors: List[Fact[?]] = self +: rep.predecessors

    override val sources: Set[Condition.Source[?]] = rep.sources

    override val alphaSources: Set[Fact.Source] = rep.alphaSources
  }

  sealed class Literal[I: FactOps] private[slips] (
    val sample: I
  ) extends Fact[I] {
    override val signature: Signature = Signature.Manual(sample.toString)

    override val predecessors: List[Fact[?]]       = List.empty
    override val sources: Set[Condition.Source[?]] = Set.empty

    override val alphaSources: Set[Fact.Source] = Set.empty
  }
  object Literal {
    case object Unit extends Literal[Unit](())
  }
  /*
  final case class Dummy[T: FactOps] private[slips] (
    src: Condition[T],
    sample: T
  ) extends Fact[T] {
    override val signature: Signature = Signature.derivedUnary(src, s => s"$s -> Fact[${ Macros.signType[T] }]")

    override def predecessors: List[Fact[?]] = List.empty

    override def sources: Set[Condition.Source[?]] = Set.empty

    override val alphaSources: Set[Source] = Set.empty
  }
   */
  sealed trait Alpha[T] extends Fact[T] {
    val sourceFact: Fact.Source
    override val isAlpha: Boolean = true

    override val predecessors: List[Fact.Alpha[?]]
  }
  object Alpha   {
    /*
    final case class Multiply[T, Q <: NonEmptyTuple](
      override val signature: Signature,
      fact: Fact.Alpha[T],
      map: T => Q
    ) extends Alpha[Q] {
      override def sourceFact: Fact.Source = fact.sourceFact

      override def source: Signature = fact.source

      override def predecessors: List[Fact.Alpha[?]] = fact +: fact.predecessors
    }
     */
    final case class Map[T, Q: FactOps](pred: Fact.Alpha[T], map: T => Q, mapSign: Signature) extends Alpha[Q] {
      override val sourceFact: Fact.Source = pred.sourceFact

      override val predecessors: List[Fact.Alpha[?]] = pred +: pred.predecessors

      override val signature: Signature = Signature.DerivedBinary(pred.signature, mapSign, (s1, s2) => s"$s1 -> $s2")
      override val sample: Q            = map(pred.sample)
      override val sources: Set[Condition.Source[?]] = sourceFact.sources
    }

    final class Source[T : FactOps : NotTuple] private (
      override val signature: Signature,
      override val sample: T,
      override val sources: Set[Condition.Source[?]]
    ) extends Alpha[T] { self =>
      override val sourceFact: Source[T] = self

      override val alphaSources: Set[Source[?]]      = Set(self)
      override val predecessors: List[Fact.Alpha[?]] = List(self)
    }

    object Source {
      def apply[T](source: Condition.Source[T])(using T: FactOps[T]): Source[T] =
        new Source(source.signature, T.empty, Set(source))
    }
  }

  object CanBeLiteral {
    given [T <: NonEmptyTuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}

    given [T](using NotTuple[T], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}
  }

  extension [T](f: Fact.Val[T]) {
    private[slips] def sources(using T: FactOps[T]): Set[Condition.Source[?]] = T.sources(f)
    private[slips] def signature(using T: FactOps[T]): Signature              = T.extract(f)
    private[slips] def alphaSources(using T: FactOps[T]): Set[Fact.Source]    = T.alphaSources(f)
    private[slips] def facts(using T: FactOps[T]): List[Fact[?]]              = T.facts(f)
    private[slips] def predecessors(using T: FactOps[T]): List[Fact[?]]       = T.predecessors(f)
  }
}
