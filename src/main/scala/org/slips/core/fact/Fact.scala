package org.slips.core.fact

import FactOps.TupleOps
import cats.Id
import cats.Monoid
import cats.data.IndexedStateT
import org.slips.NotTuple
import org.slips.core
import org.slips.core.Macros
import org.slips.core.Signed
import org.slips.core.build.BuildContext
import org.slips.core.build.BuildStep
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.*
import org.slips.core.fact.Fact.Predecessors
import org.slips.core.network.AlphaNode
import org.slips.core.network.materialized.Publisher
import org.slips.core.predicates.Predicate
import scala.Tuple.Append
import scala.Tuple.Head
import scala.Tuple.Size
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[T] extends Signed {

  override def signature: String = s"${ Macros.signType[this.type] }[${ Macros.signType[T] }]"

  inline def test(inline f: T => Boolean)(using FactOps[T]): Predicate

  @targetName("repNotEq")
  inline def =!=(other: Fact[T])(using
    TupleOps[T *: T *: EmptyTuple],
    Fact[T] =:= Fact.Val[T]
  ): Predicate = ???
  // Predicate.BetaTest(this, other, _ != _)

  @targetName("repEq")
  inline def ===(other: Fact[T])(using
    TupleOps[T *: T *: EmptyTuple],
    Fact[T] =:= Fact.Val[T]
  ): Predicate = ???
  // Predicate.BetaTest(this, other, _ == _)

  def predecessors: Predecessors
  lazy val alphaSources: Set[Fact.Source] = predecessors.collect { case x: Fact.Source => x }
  lazy val betaSources: Set[Fact[_]]      = predecessors.filter { case x: Fact.Source => false }

  def sources: Set[Condition.Source[_]]

}

object Fact {

  type Predecessors = List[Fact[_]]
  type Source       = Fact.Alpha.Source[_]
  object Predecessors {
    val empty = Set.empty[Fact[_]]
  }

  type TMap        = [x <: Tuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: Tuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped   = [x <: Tuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match
    case x <:< Tuple => TMap[x]
    case _           => Fact[X]

  type ReverseVal[X] = X match
    case x <:< Tuple => TInverseMap[x]
    case Fact[x]     => x

  val unit: Fact[Unit] = literal(())

  def literal[T : CanBeLiteral : FactOps](v: T): Fact[T] = Literal(v)

  sealed trait CanBeLiteral[T]

  sealed trait Beta[T] extends Fact[T] {
    override inline def test(inline f: T => Boolean)(using FactOps[T]): Predicate = ??? // = Predicate.BetaTest.fromScalar(this, f)
  }
  object Beta {
    final case class Tuples[T <: NonEmptyTuple] private[slips] (
      override val signature: String,
      facts: Val[T]
    )(using T: TupleOps[T]) extends Fact[T] with Beta[T] {
      override val predecessors: Predecessors        = facts.predecessors
      override val sources: Set[Condition.Source[_]] = facts.sources

    }

    final case class MapTupleToScalar[T <: NonEmptyTuple, Q](
      override val signature: String,
      pred: Fact.Val[T],
      map: T => Q
    )(using T: TupleOps[T]) extends Fact[Q] with Beta[Q] {
      override def predecessors: Predecessors = pred.predecessors

    }
  }

  sealed trait Alpha[T] extends Fact[T] {

    val source: Condition.Source[_]
    val sourceFact: Fact.Source
    override val betaSources: Set[Fact[_]]      = Set.empty
    override lazy val alphaSources: Set[Source] = Set(sourceFact)

    override def sources: Set[Condition.Source[_]] = alphaSources

    override inline def test(inline f: T => Boolean)(using FactOps[T]): Predicate = Predicate
      .AlphaTest
      .fromScalar(this, f)
  }
  object Alpha {

    final case class Map[T, Q](
      override val signature: String,
      pred: Fact.Alpha[T],
      map: T => Q
    ) extends Alpha[Q] {
      override val sourceFact: Fact.Source     = pred.sourceFact
      override val source: Condition.Source[_] = sourceFact.source
      override def predecessors: Predecessors  = pred +: pred.predecessors

    }

    final class Source[T: NotTuple] private (
      override val signature: String,
      val source: Condition.Source[T]
    )(using T: FactOps[T]) extends Alpha[T] {

      override val predecessors: Predecessors = List.empty
      override val sourceFact: Fact.Source    = this

      def buildAlphaNode: BuildStep[AlphaNode.Source[T]] = source.build

    }

    object Source {
      def apply[T : NotTuple : FactOps](source: Condition.Source[T]): Source[T] =
        new Source(source.signature, source)
    }
  }
  final case class Literal[I: CanBeLiteral] private[slips] (value: I) extends Fact[I] {
    override lazy val signature: String = value.toString

    override def predecessors: Predecessors        = Predecessors.empty
    override def sources: Set[Condition.Source[_]] = Set.empty
  }

  final case class Dummy[T] private[slips] (src: Condition[T]) extends Fact[T] {
    override val signature: String = s"${ src.signature } -> Fact[${ Macros.signType[T] }]"

    override def predecessors: Predecessors = Set.empty

    override def sources: Set[Condition.Source[_]] = Set.empty
  }

  object CanBeLiteral {
    given [T <: Tuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] = new CanBeLiteral[T] {}

    given [T](using NotGiven[T <:< Tuple], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] = new CanBeLiteral[T] {}
  }

}
