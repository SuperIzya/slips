package org.slips.core.fact

import org.slips.NotTuple
import org.slips.Signature
import org.slips.core
import org.slips.core.Macros
import org.slips.core.Signed
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.*
import scala.util.NotGiven

sealed trait Fact[T <: Any : NotTuple](using T: FactOps[T]) extends Signed { self =>

  lazy val sourceFacts: Set[Fact.Source[_]] = predecessors.collect { case x: Fact.Source[_] => x }
  val sample: T

  override def signature: Signature =
    Signature.Manual(s"${ Macros.signType[self.type] }[${ Macros.signType[T] }]($sample)")

  def predecessors: Set[Fact[_]]

  def sources: Set[Condition.Source[_]]

}

object Fact {

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
  ) extends Fact[Q] {
    override val sample: Q                  = f(rep.sample)
    override def predecessors: Set[Fact[_]] = rep.predecessors + this

    override def sources: Set[Condition.Source[_]] = rep.sources
  }

  sealed class Literal[I: FactOps] private[slips] (
    val sample: I
  ) extends Fact[I] {
    override lazy val signature: Signature = Signature.Manual(sample.toString)

    override def predecessors: Set[Fact[_]]        = Set.empty
    override def sources: Set[Condition.Source[_]] = Set.empty
  }
  object Literal {
    case object Unit extends Literal[Unit](())
  }

  final case class Dummy[T: FactOps] private[slips] (
    src: Condition[T],
    sample: T
  ) extends Fact[T] {
    override val signature: Signature = Signature.derivedUnary(src, s => s"$s -> Fact[${ Macros.signType[T] }]")

    override def predecessors: Set[Fact[_]] = Set.empty

    override def sources: Set[Condition.Source[_]] = Set.empty
  }

  final class Source[T : FactOps : NotTuple] private (
    signature: Signature,
    override val sample: T,
    override val sources: Set[Condition.Source[_]]
  ) extends Fact[T] {

    override lazy val sourceFacts: Set[Source[_]] = Set(this)
    override val predecessors: Set[Fact[_]]       = Set(this)
  }
  object Source  {
    def apply[T](source: Condition.Source[T])(using T: FactOps[T]): Source[T] =
      new Source(source.signature, T.empty, Set(source))
  }

  object CanBeLiteral {
    given [T <: NonEmptyTuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}

    given [T](using NotTuple[T], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}
  }

}
