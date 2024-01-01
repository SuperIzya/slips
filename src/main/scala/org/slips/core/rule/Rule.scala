package org.slips.core.rule

import cats.Monad
import cats.data.StateT
import cats.syntax.all.*
import org.slips.Env
import org.slips.Environment
import org.slips.core.build.Builder
import org.slips.core.build.SelectedPredicatesAndSources
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import scala.compiletime.asMatchable

sealed trait Rule[F[_], T](using F: Monad[F], T: FactOps[T]) extends Rule.RuleM { self =>

  type ThisRule  = self.type
  type Action[q] = StateT[F, Context, q]
  type Facts     = Fact.Val[T]

  val selfAction: self.Val[T] => self.Action[Unit]

  val condition: Condition[T]
  val name: String

  override private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources] =
    Builder.selectPredicatesAndSources(condition)

  case class Context(values: T, asserted: Seq[Any], retracted: Seq[Fact[?]]) { self =>

    def addFact[Q](t: Q)(using ThisRule): F[(Context, Unit)] =
      F.pure(copy(asserted = asserted :+ t) -> ())

    def remove[Q](x: Value[Q])(using ThisRule): F[(Context, Unit)] =
      F.pure(copy(retracted = retracted :+ x.fact) -> ())

    def removeAll[Q](t: Val[Q])(using ThisRule): F[(Context, Unit)] = ??? /*t match {
      case _: EmptyTuple                                      => F.pure(this -> ())
      case (x: Value[?] @unchecked) *: (v: Val[?] @unchecked) => remove(x) >> removeAll(v)
      case x: Value[Q] @unchecked                             => remove(x)
    }*/

  }

  sealed trait Value[Q](using inFacts: InTuple[Facts, Fact[Q]], inVals: InTuple[T, Q])
      extends Matchable {
    val fact: Fact[Q]
  }

  object Value {
    extension [Q](v: Value[Q]) {
      def value: ThisRule ?=> Action[Q]        = ???
      def remove(using ThisRule): Action[Unit] = StateT(_.remove(v))
    }

    type Val[Tp] = Tp match {
      case h *: EmptyTuple.type => Value[h] *: EmptyTuple
      case h *: t               => Value[h] *: Val[t]
      case _                    => Value[Tp]
    }
  }

  type Val[x] = Value.Val[x]

  type Method[T] = Val[T] => Action[Unit]

}

object Rule {

  type RuleAction[F[_], T] = (r: Rule[F, T]) ?=> r.Method[T]

  private final class Impl[F[_]: Monad, T: FactOps](
    override val name: String,
    override val condition: Condition[T],
    actions: (r: Rule[F, T]) ?=> r.Method[T]
  ) extends Rule[F, T] { self =>
    val selfAction: self.Method[T] = actions(using self)

  }

  def apply[T: FactOps](using env: Environment)(
    name: String,
    condition: Condition[T],
    actions: RuleAction[env.Effect, T]
  ): env.Rule[T] =
    new Impl[env.Effect, T](name, condition, actions)

  sealed trait RuleM {
    private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources]
  }

}
