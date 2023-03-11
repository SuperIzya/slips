package org.slips.core.fact

import FactOps.TupleOps
import cats.Id
import cats.Monoid
import cats.data.IndexedStateT
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

sealed trait Fact[T](
  val sample: T
)(using T: FactOps[T]
) extends Signed {

  lazy val toVal: Fact.Val[T] = T.toVal(this)

  override def signature: String = s"${ Macros.signType[this.type] }[${ Macros.signType[T] }]($sample)"

  inline def test(
    inline f: T => Boolean
  ): Predicate = Predicate.Test.fromFact(this, f)

  @targetName("repNotEq")
  inline def =!=(
    other: Fact[T]
  )(using
    TupleOps[
      (
        T,
        T
      )
    ],
    Fact[T] =:= Fact.Val[T]
  ): Predicate =
    Predicate.Test(this, other, _ != _)

  @targetName("repEq")
  inline def ===(
    other: Fact[T]
  )(using
    TupleOps[
      (
        T,
        T
      )
    ],
    Fact[T] =:= Fact.Val[T]
  ): Predicate =
    Predicate.Test(this, other, _ == _)

  def predecessors: Set[Fact[_]]
  lazy val sourceFacts: Set[Fact.Source[_]] = predecessors.collect { case x: Fact.Source[_] => x }
  def sources: Set[Condition.Source[_]]

}

object Fact {

  type TMap        = [x <: Tuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: Tuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped   = [x <: Tuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match
    case Tuple => TMap[X]
    case _     => Fact[X]

  type ReverseVal[X] = X match
    case Tuple   => TInverseMap[X]
    case Fact[x] => x

  val unit: Fact[Unit] = literal(())

  def fromFactTuple[T <: NonEmptyTuple, Q](
    f: Fact[T],
    extract: T => Q,
    index: Int
  )(using Q: FactOps[Q]
  ): Fact[Q] = ExtractFromTuple(
    s"${ f.signature }($index of ${ Q.size })[${ Q.signature }]",
    f,
    extract,
    Q.empty
  )

  def fromTuple[T <: NonEmptyTuple](
    t: TMap[T]
  )(using T: FactOps.TupleOps[T]
  ): Fact[T] =
    Tuples(
      T.extract(t).mkString("(", ", ", ")"),
      t,
      T.empty
    )

  def literal[T : CanBeLiteral : FactOps](
    v: T
  ): Fact[T] = Literal(v)

  sealed trait CanBeLiteral[T]

  final case class ExtractFromTuple[T <: NonEmptyTuple, Q: FactOps] private[slips] (
    override val signature: String,
    src: Fact[T],
    extract: T => Q,
    override val sample: Q
  ) extends Fact[Q](sample) {
    override val predecessors: Set[Fact[_]]        = src.predecessors + src
    override val sources: Set[Condition.Source[_]] = src.sources
  }

  final case class Tuples[T <: NonEmptyTuple] private[slips] (
    override val signature: String,
    facts: TMap[T],
    override val sample: T
  )(using T: TupleOps[T]
  ) extends Fact[T](sample) {
    override val predecessors: Set[Fact[_]]        = T.predecessors(facts)
    override val sources: Set[Condition.Source[_]] = T.sources(facts)

  }
  final case class Map[T, Q: FactOps](
    override val signature: String,
    f: T => Q,
    rep: Fact[T]
  ) extends Fact[Q](f(rep.sample)) {
    override def predecessors: Set[Fact[_]] = rep.predecessors + rep

    override def sources: Set[Condition.Source[_]] = rep.sources
  }

  final case class Literal[I: FactOps] private[slips] (
    value: I
  ) extends Fact[I](value) {
    override lazy val signature: String = value.toString

    override def predecessors: Set[Fact[_]]        = Set.empty
    override def sources: Set[Condition.Source[_]] = Set.empty
  }

  final case class Dummy[T: FactOps] private[slips] (
    src: Condition[T],
    override val sample: T
  ) extends Fact[T](sample) {
    override val signature: String =
      s"${ src.signature } -> Fact[${ Macros.signType[T] }]"

    override def predecessors: Set[Fact[_]] = Set.empty

    override def sources: Set[Condition.Source[_]] = Set.empty
  }

  final class Source[T: FactOps] private (
    override val signature: String,
    override val sample: T,
    override val sources: Set[Condition.Source[_]]
  ) extends Fact[T](sample) {

    override val predecessors: Set[Fact[_]]       = Set.empty
    override lazy val sourceFacts: Set[Source[_]] = Set(this)
  }
  object Source {
    def apply[T](
      source: Condition.Source[T]
    )(using T: FactOps[T]
    ): Source[T] =
      new Source(source.signature, T.empty, Set(source))
  }

  object CanBeLiteral {
    given [T <: Tuple](
      using NotGiven[TIsMapped[T]]
    ): CanBeLiteral[T] =
      new CanBeLiteral[T] {}

    given [T](
      using NotGiven[T <:< Tuple],
      NotGiven[T =:= Fact[?]]
    ): CanBeLiteral[T] =
      new CanBeLiteral[T] {}
  }

}
