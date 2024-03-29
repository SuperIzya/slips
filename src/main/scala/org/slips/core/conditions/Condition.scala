package org.slips.core.conditions

import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.BuildStep
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.network.AlphaNode
import org.slips.core.network.Node
import org.slips.core.predicates.Predicate
import scala.annotation.targetName

sealed trait Condition[T] extends Signed {
  override val signature: String = ""
  private[slips] val parse: ParseStep[T]

  def flatMap[Q](
    f: Fact.Val[T] => Condition[Q]
  ): Condition[Q] =
    Condition.FlatMap[T, Q](this, f)

  def map[R, Q](
    f: Fact.Val[T] => R
  )(using
    ev: Fact.ReverseVal[R] =:= Q,
    ev2: R =:= Fact.Val[Q]
  ): Condition[Q] =
    Condition.map[T, Q](this, f.andThen(ev2(_)))

  def withFilter(
    f: Fact.Val[T] => Predicate
  ): Condition[T] =
    Condition.Filter(this, f)

  @targetName("withFilterSingle")
  def withFilter(
    f: Fact.Val[T] => Boolean
  ): Condition[T] =
    Condition.ScalarFilter(this, f)
}

object Condition {
  type Res[x] = Environment ?=> Condition[x]

  inline def all[T : FactOps : NotTuple]: All[T] = {
    All[T](s"All[${ Macros.signType[T] }]")
  }

  private def map[T, Q](
    src: Condition[T],
    f: Fact.Val[T] => Fact.Val[Q]
  ): Map[T, Q] =
    Map(src, f)

  sealed trait Source[T : FactOps : NotTuple] extends Condition[T] {
    private def fact: Fact.Source[T]                = Fact.Source(this)
    override private[slips] val parse: ParseStep[T] = ParseStep.modify(_.addSource(this)).map(_ => fact.toVal)

    private[slips] def build: BuildStep[Node] = BuildStep.addNode(AlphaNode.Source(this))
  }

  final case class All[T : FactOps : NotTuple] private[Condition] (
    override val signature: String
  ) extends Source[T]

  final case class OpaquePredicate private[slips] (
    p: Predicate
  ) extends Condition[Unit] {
    override val signature: String = p.signature

    override private[slips] val parse: ParseStep[Unit] = Predicate.add(p.toKNF)

  }

  final case class Map[T, Q] private[Condition] (
    src: Condition[T],
    f: Fact.Val[T] => Fact.Val[Q]
  ) extends Condition[Q] {
    override val signature: String = ""

    override private[slips] val parse: ParseStep[Q] = src.parse.map(f)
  }

  private[slips] final case class FlatMap[T, Q](
    left: Condition[T],
    f: Fact.Val[T] => Condition[Q]
  ) extends Condition[Q] {
    override private[slips] val parse: ParseStep[Q] =
      left.parse.flatMap(f(_).parse)
  }

  private[slips] final case class ScalarFilter[T](
    src: Condition[T],
    f: Fact.Val[T] => Boolean
  ) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = src
      .parse
      .map {
        case x if f(x) => x
      }
  }

  private[slips] final case class Filter[T](
    cond: Condition[T],
    f: Fact.Val[T] => Predicate
  ) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = for {
      t <- cond.parse
      predicate = f(t)
      _ <- Predicate.add(predicate.toKNF)
    } yield t
  }

}
