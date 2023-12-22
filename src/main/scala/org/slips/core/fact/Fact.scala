package org.slips.core.fact

import FactOps.TupleOps
import cats.Id
import cats.Monoid
import cats.data.IndexedStateT
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core
import org.slips.core.Macros
import org.slips.core.Signed
import org.slips.core.build.BuildContext
import org.slips.core.build.BuildStep
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.*
import org.slips.core.network.materialized.Publisher
import org.slips.core.predicates.Predicate
import scala.Tuple.Append
import scala.Tuple.Head
import scala.Tuple.Size
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[T: NotTuple](using T: FactOps[T]) extends Signed { self =>
  val sample: T

  override def signature: Signature         =
    Signature.Manual(s"${ Macros.signType[self.type] }[${ Macros.signType[T] }]($sample)")
  def predecessors: Set[Fact[_]]
  lazy val sourceFacts: Set[Fact.Source[_]] = predecessors.collect { case x: Fact.Source[_] => x }
  def sources: Set[Condition.Source[_]]

}

object Fact {

  type TMap        = [x <: NonEmptyTuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: NonEmptyTuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped   = [x <: NonEmptyTuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match
    case t <:< NonEmptyTuple => TMap[t]
    case _                   => Fact[X]

  type ReverseVal[X] = X match
    case t <:< NonEmptyTuple => TInverseMap[t]
    case Fact[x]             => x

  val unit: Fact[Unit] = literal(())

  def literal[T : CanBeLiteral : FactOps](v: T): Fact[T] = Literal(v)

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

  final case class Literal[I: FactOps] private[slips] (
    sample: I
  ) extends Fact[I] {
    override lazy val signature: Signature = Signature.Manual(sample.toString)

    override def predecessors: Set[Fact[_]]        = Set.empty
    override def sources: Set[Condition.Source[_]] = Set.empty
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

    override val predecessors: Set[Fact[_]]       = Set(this)
    override lazy val sourceFacts: Set[Source[_]] = Set(this)
  }
  object Source {
    def apply[T](source: Condition.Source[T])(using T: FactOps[T]): Source[T] =
      new Source(source.signature, T.empty, Set(source))
  }

  object CanBeLiteral {
    given [T <: Tuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}

    given [T](using NotTuple[T], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] =
      new CanBeLiteral[T] {}
  }

}
