package org.slips.core.rule

import cats.Monad
import cats.data.StateT
import org.slips.Env
import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.build.Builder
import org.slips.core.build.SelectedPredicatesAndSources
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps

sealed trait Rule[F[_], T: FactOps](using F: Monad[F]) extends Rule.RuleM {

  type ThisRule  = this.type
  type Action[q] = StateT[F, Context, q]
  type Facts     = Fact.Val[T]

  def condition: Condition[T]
  def name: String

  override private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources] =
    Builder.selectPredicatesAndSources(condition)

  case class Context(values: T, asserted: Seq[Any], retracted: Seq[Fact[_]]) {

    def addFact[Q](t: Q)(using ThisRule): F[(Context, Unit)] = F.pure(copy(asserted = asserted :+ t) -> ())

    def remove[Q](t: Value.Val[Q])(using ThisRule): F[(Context, Unit)] = t match {
      case EmptyTuple => F.pure(this -> ())
      case x :* v     => F.pure(copy(retracted = retracted :+ x.fact) -> ()).flatMap { _ => retract(v) }
      case x          => F.pure(copy(retracted = retracted :+ x.fact) -> ())
    }

  }

  trait Value[Q](using inFacts: InTuple[Facts, this.type], inVals: InTuple[T, Q]) {
    val fact: Fact[Q]

    def value: ThisRule ?=> Action[Q]        = ???
    def remove(using ThisRule): Action[Unit] = StateT(_.retract(this))
  }

  object Value {
    type Val[Tp] = Tp match
      case EmptyTuple => EmptyTuple
      case Tuple      => Tuple.Map[Tp, Value]
      case _          => Value[Tp]
  }

}

object Rule {
  type RuleAction[F[_], T] = (r: Rule[F, T]) ?=> r.Value.Val[T] => r.Action[Unit]

  class Builder[T: FactOps](name: String, condition: Condition[T]) {

    def withAction(
      using env: Environment
    )(
      actions: RuleAction[env.Effect, T]
    ): env.Rule[T] = ???
  }

  trait RuleM {
    private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources]
  }
}
