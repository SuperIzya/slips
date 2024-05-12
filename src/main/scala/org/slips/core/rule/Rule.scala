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

sealed trait Rule[F[_]](using F: Monad[F]) { self =>
  type T

  type ThisRule  = self.type
  type Action[q] = StateT[F, Context, q]
  type Facts     = Fact.Val[T]

  val T: FactOps[T]
  val selfAction: self.Val[T] => self.Action[Unit]

  val condition: Condition[T]
  val name: String

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

  private final class Impl[F[_]: Monad, V](
    override val name: String,
    override val condition: Condition[V],
    actions: (r: Rule[F]) ?=> r.Method[V]
  )(using val T: FactOps[V]) extends Rule[F] { self =>
    val selfAction: self.Method[T] = actions(using self)
    type T = V
  }

  def apply[T: FactOps](using env: Environment)(
    name: String,
    condition: Condition[T],
    actions: (rule: Rule[env.Effect]) ?=> rule.Method[T]
  ): env.Rule =
    new Impl[env.Effect, T](name, condition, actions)

}
