package org.slips.core.fact

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
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.fact.Fact.*
import org.slips.core.fact.Fact.Predecessors
import org.slips.core.network.alpha.AlphaNode
import org.slips.core.network.materialized.Publisher
import scala.Tuple.Append
import scala.Tuple.Head
import scala.Tuple.Size
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[+T: NotTuple] extends WithSignature {

  lazy val (alphaSources: Set[Fact.Source], betaSources: Set[Fact[?]]) = predecessors
    .foldLeft((Set.empty[Fact.Source], Set.empty[Fact[?]])) { (col, p) =>
      if (p.isAlpha) (col._1 ++ p.alphaSources, col._2)
      else col._1 -> (p.betaSources ++ col._2)
    }: @unchecked
  private[slips] val isAlpha: Boolean

  def predecessors: Predecessors

  def sources: Set[Signature] = alphaSources.map(_.source)

  override def signature: Signature = Signature.Manual(s"${ Macros.signType[this.type] }[${ Macros.signType[T] }]")

}

object Fact {

  type Source           = Fact.Alpha.Source[?]
  type TMap[T <: Tuple] = Tuple.Map[T, Fact]
  type TInverseMap      = [x <: Tuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped        = [t <: Tuple] =>> Tuple.IsMappedBy[Fact][t]

  type Val[X] = Fact[X] | TMap[X]

  type InverseVal[X] = X match {
    case EmptyTuple.type        => EmptyTuple
    case Fact[h] *: Fact.Val[t] => h *: InverseVal[t]
    case Fact[t]                => t
  }

  val unit: Fact[Unit] = literal(())

  def literal[T : CanBeLiteral : FactOps](v: T): Fact[T] = Literal(v)

  sealed trait CanBeLiteral[T]

  sealed trait Beta[T] extends Fact[T] {

    override private[slips] val isAlpha = false

  }

  sealed trait Alpha[T] extends Fact[T] {

    override lazy val betaSources: Set[Fact[?]] = Set.empty
    override lazy val alphaSources: Set[Source] = Set(sourceFact)
    private[slips] override val isAlpha         = true

    def source: Signature

    def sourceFact: Fact.Source

    override def predecessors: List[Fact.Alpha[?]]

    override def sources: Set[Signature] = Set(sourceFact.source)

  }

  final case class Literal[I: CanBeLiteral] private[slips] (value: I) extends Fact[I] {
    override lazy val signature: Signature = Signature.Manual(value.toString)

    override private[slips] val isAlpha = true

    override def predecessors: Predecessors = Predecessors.empty
    override def sources: Set[Signature]    = Set(signature)
  }

  final case class Dummy[T] private[slips] (src: Condition[T]) extends Fact[T] {
    override val signature: Signature = Signature.Manual(s"DummyFact[${ Macros.signType[T] }]")

    override private[slips] val isAlpha = true

    override def predecessors: Predecessors = Predecessors.empty

    override def sources: Set[Signature] = Set.empty
  }

  object Predecessors {
    val empty = List.empty[Fact[?]]
  }

  object Beta {
    final case class Map[T, Q](pred: Fact.Val[T], map: T => Q, mapSign: Signature)(using T: FactOps[T])
        extends Beta[Q] {
      override def sources: Set[Signature] = ???

      override def predecessors: List[Fact[?]] = List.empty

      override def signature: Signature = Signature.DerivedBinary(pred.signature, mapSign, (s1, s2) => s"$s1 -> $s2")

      def signed(signature: Signature): Map[T, Q] = copy(mapSign = signature)
    }
  }

  object Alpha {
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
    final case class Map[T, Q](pred: Fact.Alpha[T], map: T => Q, mapSign: Signature) extends Alpha[Q] {
      override val sourceFact: Fact.Source           = pred.sourceFact
      override val source: Signature                 = sourceFact.source
      override def predecessors: List[Fact.Alpha[?]] = pred +: pred.predecessors

      override def signature: Signature = Signature.DerivedBinary(pred.signature, mapSign, (s1, s2) => s"$s1 -> $s2")

      def signed(signature: Signature): Map[T, Q] = copy(mapSign = signature)
    }

    final class Source[T] private (val source: Signature) extends Alpha[T] {

      override val signature: Signature = source

      override val predecessors: List[Fact.Alpha[?]] = List.empty
      override val sourceFact: Fact.Source           = this

      override def sources: Set[Signature] = Set(source)
    }

    object Source {
      def apply[T](source: Condition.Source[T]): Source[T] =
        new Source(source.signature)
    }
  }

  object CanBeLiteral extends CanBeLiteral.LowPrio {

    given [T <: Tuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] = new CanBeLiteral[T] {}

    given [T](using NotTuple[T], NotGiven[T =:= Fact[?]]): CanBeLiteral[T] = new CanBeLiteral[T] {}

    trait LowPrio {
      given unitCanBeLiteral: CanBeLiteral[Unit] = new CanBeLiteral[Unit] {}
    }
  }

}
