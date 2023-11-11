package org.slips.core.rule

import cats.Monad
import cats.data.StateT
import cats.syntax.all.*
import compiletime.asMatchable
import org.slips.Env
import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.build.Builder
import org.slips.core.build.SelectedPredicatesAndSources
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps

sealed trait Rule[F[_], T: FactOps](using F: Monad[F]) extends Rule.RuleM { self =>

  type ThisRule  = self.type
  type Action[q] = StateT[F, Context, q]
  type Facts     = Fact.Val[T]

  def condition: Condition[T]
  def name: String

  override private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources] =
    Builder.selectPredicatesAndSources(condition)

  case class Context(values: T, asserted: Seq[Any], retracted: Seq[Fact[?]]) { self =>

    def addFact[Q](t: Q)(using ThisRule): F[(Context, Unit)] =
      F.pure(copy(asserted = asserted :+ t) -> ())

    def remove[Q](x: Value[Q])(using ThisRule): F[(Context, Unit)] =
      F.pure(copy(retracted = retracted :+ x.fact) -> ())

    def removeAll[Q](t: Value.Val[Q])(using ThisRule): F[(Context, Unit)] = ??? /*t.asMatchable match {
      case _: EmptyTuple                      => F.pure(self -> ())
      case (x: Value[?]) *: (v: Value.Val[?]) => remove(x) >> removeAll(v)
      case x: Value[Q]                        => remove(x)
    }*/

  }

  trait Value[Q](using inFacts: InTuple[Facts, Fact[Q]], inVals: InTuple[T, Q]) extends Matchable {
    val fact: Fact[Q]

    def value: ThisRule ?=> Action[Q]        = ???
    def remove(using ThisRule): Action[Unit] = StateT(_.remove(this))
  }

  object Value {
    type Val[Tp] = Tp match
      case h *: t => Value[h] *: Val[t]
      case _      => Value[Tp]

  }

}

object Rule {
  type RuleAction[F[_], T] = (r: Rule[F, T]) ?=> (r.Value.Val[T] => r.Action[Unit])

  private[slips] trait RuleWithAction[F[_]: Monad, T: FactOps](
    val name: String,
    override val condition: Condition[T]
  ) extends Rule[F, T] {
    def action(values: this.Value.Val[T]): this.Action[Unit]
  }

  class Builder[T: FactOps](name: String, condition: Condition[T]) {

    def withAction(using env: Environment)(actions: RuleAction[env.Effect, T]): env.Rule[T] = {
      new RuleWithAction[env.Effect, T](name, condition) { self =>
        override def action(values: self.Value.Val[T]): self.Action[Unit] = actions(using self)(values)
      }
    }
  }

  trait RuleM {
    private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources]
  }
}
