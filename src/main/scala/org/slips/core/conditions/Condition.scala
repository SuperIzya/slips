package org.slips.core.conditions

import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.BuildStep
import org.slips.core.fact.*
import org.slips.core.network.Node
import org.slips.core.network.alpha.AlphaNode
import org.slips.core.predicates.Predicate
import scala.annotation.targetName

sealed trait Condition[T] extends Signed {
  protected implicit val T: FactOps[T]
  override val signature: String = ""
  private[slips] def parse: ParseStep[T]

  def flatMap[Q](f: Fact.Val[T] => Condition[Q]): Condition[Q] =
    Condition.FlatMap[T, Q](this, f)

  def map[R, Q](f: Fact.Val[T] => R)(using ev: R =:= Fact.Val[Q]): Condition[Q] =
    Condition.map[T, Q](this, f.andThen(ev(_)))

  def withFilter(f: Fact.Val[T] => Predicate): Condition[T] =
    Condition.Filter(this, f)

  @targetName("withFilterSingle")
  def withFilter(f: Fact[T] => Boolean)(using NotTuple[T], FactOps[T], Fact.Val[T] =:= Fact[T]): Condition[T] =
    Condition.ScalarFilter(this, f)
}

object Condition {

  inline def all[T : FactOps : NotTuple]: All[T] = {
    All[T](s"All[${ Macros.signType[T] }]")
  }

  private def map[T, Q](
    src: Condition[T],
    f: Fact.Val[T] => Fact.Val[Q]
  ): Map[T, Q] =
    Map(src, f)

  sealed trait Source[T] extends Condition[T] {
    private def fact: Fact.Alpha.Source[T] = Fact.Alpha.Source(this)

    def extractNode(sourceMap: scala.collection.immutable.Map[String, AlphaNode.Source[_]]): AlphaNode.Source[T] = {
      sourceMap(signature).asInstanceOf[AlphaNode.Source[T]]
    }

    override private[slips] def parse: ParseStep[T] = ParseStep.modify(_.addSource(this)).map(_ => fact.toVal)
  }

  final case class All[T] private[Condition] (override val signature: String)(using override val T: FactOps[T])
      extends Source[T]

  final case class OpaquePredicate private[slips] (p: Predicate) extends Condition[Unit] {
    override val signature: String = p.signature

    override private[slips] val parse: ParseStep[Unit] = Predicate.add(p.toDNF)

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

  private[slips] final case class ScalarFilter[T: NotTuple](
    src: Condition[T],
    f: Fact[T] => Boolean
  )(using T: FactOps[T], ev: Fact.Val[T] =:= Fact[T]) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = src
      .parse
      .map {
        case x if f(ev(x)) => x
      }
  }

  private[slips] final case class Filter[T](
    cond: Condition[T],
    f: Fact.Val[T] => Predicate
  ) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = for {
      t <- cond.parse
      predicate = f(t)
      _ <- Predicate.add(predicate.toDNF)
    } yield t
  }

}
