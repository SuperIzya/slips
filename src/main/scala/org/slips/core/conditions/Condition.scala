package org.slips.core.conditions

import org.slips.Environment
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.BuildStep
import org.slips.core.fact.*
import org.slips.core.fact.FactOps.ScalarFact
import org.slips.core.network.Node
import org.slips.core.network.alpha.AlphaNode
import scala.annotation.targetName
import scala.compiletime.summonInline
import scala.util.NotGiven

sealed trait Condition[T] { self =>
  implicit val T: FactOps[T]

  def flatMap[Q: FactOps](f: Fact.Val[T] => Condition[Q]): Condition[Q] =
    Condition.FlatMap[T, Q](self, f)

  def map[R, Q](f: Fact.Val[T] => R)(using
    ev: R =:= Fact.Val[Q],
    Q: FactOps[Q],
    ev2: NotGiven[Q =:= Predicate]
  ): Condition[Q] =
    Condition.Map(self, f(_).asInstanceOf[Fact.Val[Q]])

  inline def map(f: Fact.Val[T] => Predicate): Condition[T] = withFilter(f)

  def withFilter(f: Fact.Val[T] => Predicate): Condition[T] =
    Condition.Filter(self, f)

  @targetName("withFilterSingle")
  def withFilter(f: Fact[T] => Boolean)(using NotTuple[T], FactOps[T], ScalarFact[T]): Condition[T] =
    Condition.ScalarFilter(self, f)

  private[slips] def parse: ParseStep[T]
}

object Condition {

  inline def all[T : FactOps : NotTuple : ScalarFact]: All[T] = {
    All[T](Signature.Manual(s"All[${ Macros.signType[T] }]"))
  }

  sealed trait Source[T : NotTuple : ScalarFact] extends Condition[T] { self =>
    private[slips] def signature: Signature

    override private[slips] def parse: ParseStep[T] = ParseStep
      .modify(_.addSource(self))
      .map(_ => summon[ScalarFact[T]].flip(fact))

    private def fact: Fact.Alpha.Source[T] = Fact.Alpha.Source(self)
  }

  final case class MapOps[T, R](self: Condition[T], f: Fact.Val[T] => R)

  private[slips] case class All[T : NotTuple : ScalarFact : FactOps](override val signature: Signature)(using
    override implicit val T: FactOps[T])
      extends Source[T]

  private[slips] case class Map[T, Q](
    src: Condition[T],
    f: Fact.Val[T] => Fact.Val[Q]
  )(using override val T: FactOps[Q])
      extends Condition[Q] {
    override private[slips] val parse: ParseStep[Q] = src.parse.map(f)
  }

  private[slips] final case class FlatMap[T, Q](
    left: Condition[T],
    f: Fact.Val[T] => Condition[Q]
  )(using override val T: FactOps[Q])
      extends Condition[Q] {
    override private[slips] val parse: ParseStep[Q] =
      left.parse.flatMap(f(_).parse)
  }

  private[slips] final case class ScalarFilter[T: NotTuple](
    src: Condition[T],
    f: Fact[T] => Boolean
  )(using override val T: FactOps[T], ev: Fact.Val[T] =:= Fact[T]) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = src
      .parse
      .map {
        case x if f(ev(x)) => x
      }
  }

  private[slips] final case class Filter[T](
    cond: Condition[T],
    f: Fact.Val[T] => Predicate
  )(using override val T: FactOps[T]) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = for {
      t <- cond.parse
      predicate = f(t)
      _ <- Predicate.add(predicate.toDNF)
    } yield t
  }

  object MapOps {
    implicit def toCondition[T, R, Q: FactOps](
      mo: MapOps[T, R])(using ev2: Fact.InverseVal[R] =:= Q, ev: R =:= Fact.Val[Q]): Condition[Q] = {
      import mo.self.T
      Condition.Map[T, Q](mo.self, mo.f.andThen(ev))
    }
  }

}
