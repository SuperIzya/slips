package org.slips.core.fact

import FactOps.TupleOps
import cats.Eq
import cats.Id
import cats.Monoid
import cats.data.IndexedStateT
import org.slips.Environment
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core
import org.slips.core.Macros
import org.slips.core.SignatureStrategy
import org.slips.core.WithSignature
import org.slips.core.build.BuildContext
import org.slips.core.build.BuildStep
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.*
import org.slips.core.fact.Fact.Predecessors
import org.slips.core.network.alpha.AlphaNode
import org.slips.core.network.materialized.Publisher
import org.slips.core.predicates.Predicate
import scala.Tuple.Append
import scala.Tuple.Head
import scala.Tuple.Size
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[T] extends WithSignature {

  def predecessors: Predecessors
  lazy val (alphaSources: Set[Fact.Source], betaSources: Set[Fact[?]]) = predecessors
    .foldLeft((Set.empty[Fact.Source], Set.empty[Fact[?]])) { (col, p) =>
      if (p.isAlpha) (col._1 ++ p.alphaSources, col._2)
      else col._1 -> (p.betaSources ++ col._2)
    }: @unchecked

  def sources: Set[Signature] = alphaSources.map(_.source)

  override def signature: Signature = s"${ Macros.signType[this.type] }[${ Macros.signType[T] }]"

  private[slips] val isAlpha: Boolean

}

object Fact {

  type Source = Fact.Alpha.Source[?]
  object Predecessors {
    val empty = List.empty[Fact[_]]
  }

  type TMap        = [T <: Tuple] =>> Tuple.Map[T, Fact]
  type TInverseMap = [T <: Tuple] =>> Tuple.InverseMap[T, Fact]
  type TIsMapped   = [T <: Tuple] =>> Tuple.IsMappedBy[Fact][T]

  type Val[X] = X match {
    case EmptyTuple      => EmptyTuple
    case q *: EmptyTuple => Fact[q] *: EmptyTuple
    case q *: t          => Fact[q] *: Val[t]
    case _               => Fact[X]
  }

  type ReverseVal[X] = X match
    case x <:< Tuple => TInverseMap[x]
    case Fact[x]     => x

  val unit: Fact[Unit] = literal(())

  def literal[T : CanBeLiteral : FactOps](v: T): Fact[T] = Literal(v)

  sealed trait CanBeLiteral[T]

  sealed trait Beta[T] extends Fact[T] {

    override private[slips] val isAlpha = false

  }
  object Beta {}

  sealed trait Alpha[T] extends Fact[T] {

    private[slips] override val isAlpha = true

    def source: Signature
    def sourceFact: Fact.Source
    override lazy val betaSources: Set[Fact[_]] = Set.empty
    override lazy val alphaSources: Set[Source] = Set(sourceFact)

    override def predecessors: List[Fact.Alpha[_]]

    override def sources: Set[Signature] = alphaSources.flatMap(_.sources)

  }
  object Alpha {

    final case class Multiply[T, Q <: NonEmptyTuple](
      override val signature: Signature,
      fact: Fact.Alpha[T],
      map: T => Q
    ) extends Alpha[Q] {
      override def sourceFact: Fact.Source = fact.sourceFact

      override def source: Signature = fact.source

      override def predecessors: List[Fact.Alpha[?]] = fact +: fact.predecessors
    }

    final case class Map[T, Q](pred: Fact.Alpha[T], map: T => Q, mapSign: Signature) extends Alpha[Q] {
      override val sourceFact: Fact.Source           = pred.sourceFact
      override val source: Signature                 = sourceFact.source
      override def predecessors: List[Fact.Alpha[?]] = pred +: pred.predecessors

      override def signature: Signature = s"${ pred.signature } -> $mapSign"

      def signed(signature: Signature): Map[T, Q] = copy(mapSign = signature)
    }

    final class Source[T : NotTuple : FactOps] private (val source: Signature) extends Alpha[T] {

      override val signature: Signature = source

      override val predecessors: List[Fact.Alpha[_]] = List.empty
      override val sourceFact: Fact.Source           = this

      def conditionSource: Condition.Source[T] = Condition.All[T](signature)

    }

    object Source {
      def apply[T : NotTuple : FactOps](source: Condition.Source[T]): Source[T] =
        new Source(source.signature)
    }
  }
  final case class Literal[I: CanBeLiteral] private[slips] (value: I) extends Fact[I] {
    override lazy val signature: Signature = value.toString

    override private[slips] val isAlpha = true

    override def predecessors: Predecessors = Predecessors.empty
    override def sources: Set[Signature]    = Set.empty
  }

  final case class Dummy[T] private[slips] (src: Condition[T]) extends Fact[T] {
    override val signature: Signature = s"${ src.signature } -> Fact[${ Macros.signType[T] }]"

    override private[slips] val isAlpha = true

    override def predecessors: Predecessors = Predecessors.empty

    override def sources: Set[Signature] = Set.empty
  }

  object CanBeLiteral {
    given [T <: Tuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] = new CanBeLiteral[T] {}

    given [T](using NotGiven[T <:< Tuple], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] = new CanBeLiteral[T] {}
  }

}
