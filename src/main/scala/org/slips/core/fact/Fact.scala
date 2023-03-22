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
import org.slips.core.network.alpha.AlphaNode
import org.slips.core.network.materialized.Publisher
import org.slips.core.predicates.Predicate
import scala.Tuple.Append
import scala.Tuple.Head
import scala.Tuple.Size
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[T] extends Signed {
  private inline def buildPredicate(other: Fact[T], inline test: (T, T) => Boolean)(using
    TT: TupleOps[T *: T *: EmptyTuple]
  ): Predicate = {
    (isAlpha, other.isAlpha) match {
      case (true, true) if other.alphaSources == alphaSources =>
        Predicate.Test(
          Fact.Alpha.Same(source = alphaSources.head, collected = TT.toVal(this -> other)),
          test.tupled
        )(test)
      case _                                                  => ???
    }
  }
  @targetName("repNotEq")
  inline def =!=(other: Fact[T])(using TT: TupleOps[T *: T *: EmptyTuple], F: Fact[T] =:= Fact.Val[T]): Predicate =
    buildPredicate(other, _ != _)

  @targetName("repEq")
  inline def ===(other: Fact[T])(using TupleOps[T *: T *: EmptyTuple], Fact[T] =:= Fact.Val[T]): Predicate =
    buildPredicate(other, _ == _)

  def predecessors: Predecessors
  lazy val (alphaSources: Set[Fact.Source], betaSources: Set[Fact[_]]) = predecessors
    .foldLeft((Set.empty[Fact.Source], Set.empty[Fact[_]])) { (col, p) =>
      if (p.isAlpha) (col._1 ++ p.alphaSources, col._2)
      else col._1 -> (p.betaSources ++ col._2)
    }

  def sources: Set[Condition.Source[_]] = alphaSources.map(_.source)

  override def signature: String = s"${ Macros.signType[this.type] }[${ Macros.signType[T] }]"

  private[slips] val isAlpha: Boolean

}

object Fact {

  type Source = Fact.Alpha.Source[_]
  object Predecessors {
    val empty = List.empty[Fact[_]]
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

    override private[slips] val isAlpha = false

  }
  object Beta {}

  sealed trait Alpha[T] extends Fact[T] {

    private[slips] override val isAlpha = true

    val source: Condition.Source[_]
    val sourceFact: Fact.Source
    override val betaSources: Set[Fact[_]]      = Set.empty
    override lazy val alphaSources: Set[Source] = Set(sourceFact)

    override def predecessors: List[Fact.Alpha[_]]

    override def sources: Set[Condition.Source[_]] = alphaSources.flatMap(_.sources)

  }
  object Alpha {

    final case class Same[Q <: NonEmptyTuple : TupleOps](
      source: Fact.Source,
      collected: Fact.Val[Q]
    ) extends Alpha[Q] {
      override def signature: String                 = s"(${ source.signature }, ${ source.signature })"
      override lazy val predecessors: List[Alpha[_]] =
        collected.facts.map(_.asInstanceOf[Alpha[_]]).toList ++ collected.predecessors
    }

    final case class Multiply[T, Q <: NonEmptyTuple](
      override val signature: String,
      fact: Fact.Alpha[T],
      map: T => Q
    ) extends Alpha[Q] {
      override def predecessors: Predecessors = fact +: fact.predecessors
    }

    final case class Map[T, Q](pred: Fact.Alpha[T], map: T => Q, sign: String) extends Alpha[Q] {
      override def signature: String           = sign
      override val sourceFact: Fact.Source     = pred.sourceFact
      override val source: Condition.Source[_] = sourceFact.source
      override def predecessors: Predecessors  = pred +: pred.predecessors
    }
    object Map {
      inline def apply[T, Q](fact: Fact.Alpha[T], inline map: T => Q): Map[T, Q] =
        Macros.createSigned(
          new Map(fact, map, _),
          map
        )
    }

    final class Source[T : NotTuple : FactOps] private (val source: Condition.Source[T]) extends Alpha[T] {

      override val signature: String = s"${ source.signature }@${ this.hashCode() }"

      override val predecessors: List[Fact.Alpha[_]] = List.empty
      override val sourceFact: Fact.Source           = this

    }

    object Source {
      def apply[T : NotTuple : FactOps](source: Condition.Source[T]): Source[T] =
        new Source(source)
    }
  }
  final case class Literal[I: CanBeLiteral] private[slips] (value: I) extends Fact[I] {
    override lazy val signature: String = value.toString

    override def predecessors: Predecessors        = Predecessors.empty
    override def sources: Set[Condition.Source[_]] = Set.empty
  }

  final case class Dummy[T] private[slips] (src: Condition[T]) extends Fact[T] {
    override val signature: String = s"${ src.signature } -> Fact[${ Macros.signType[T] }]"

    override def predecessors: Predecessors = Predecessors.empty

    override def sources: Set[Condition.Source[_]] = Set.empty
  }

  object CanBeLiteral {
    given [T <: Tuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] = new CanBeLiteral[T] {}

    given [T](using NotGiven[T <:< Tuple], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] = new CanBeLiteral[T] {}
  }

}
