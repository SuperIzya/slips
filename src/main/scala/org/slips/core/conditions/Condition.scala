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
import org.slips.core.predicates.Predicate
import scala.annotation.targetName
import scala.compiletime.summonInline

sealed trait Condition[T] extends WithSignature {
  protected implicit val T: FactOps[T]
  override val signature: Signature = Signature.Manual("")
  private[slips] def parse: ParseStep[T]

  def flatMap[Q: FactOps](f: Fact.Val[T] => Condition[Q]): Condition[Q] =
    Condition.FlatMap[T, Q](this, f)

  def map[R](f: Fact.Val[T] => R)(using ev: FactOps[Fact.InverseVal[R]]): Condition[Fact.InverseVal[R]] =
    Condition.Map(this, f.andThen(_.asInstanceOf[Fact.Val[Fact.InverseVal[R]]]))

  def withFilter(f: Fact.Val[T] => Predicate): Condition[T] =
    Condition.Filter(this, f)

  @targetName("withFilterSingle")
  def withFilter(f: Fact[T] => Boolean)(using NotTuple[T], FactOps[T], ScalarFact[T]): Condition[T] =
    Condition.ScalarFilter(this, f)
}

object Condition {

  final case class MapOps[T, R](self: Condition[T], f: Fact.Val[T] => R)
  object MapOps {
    implicit def toCondition[T, R, Q: FactOps](
      mo: MapOps[T, R])(using ev2: Fact.InverseVal[R] =:= Q, ev: R =:= Fact.Val[Q]): Condition[Q] = {
      import mo.self.T
      Condition.Map[T, Q](mo.self, mo.f.andThen(ev))
    }
  }

  inline def all[T : FactOps : NotTuple]: All[T] = {
    All[T](Signature.Manual(s"All[${ Macros.signType[T] }]"))
  }

  sealed trait Source[T] extends Condition[T] {
    private def fact: Fact.Alpha.Source[T] = Fact.Alpha.Source(this)

    override private[slips] def parse: ParseStep[T] = ParseStep.modify(_.addSource(this)).map(_ => fact.toVal)
  }

  private[slips] case class All[T](override val signature: Signature)(using override val T: FactOps[T])
      extends Source[T]

  private[slips] case class OpaquePredicate(p: Predicate)(using override val T: FactOps[Unit]) extends Condition[Unit] {

    override val signature: Signature = p.signature

    override private[slips] val parse: ParseStep[Unit] = Predicate.add(p.toDNF)

  }

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

}
