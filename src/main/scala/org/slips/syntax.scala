package org.slips

import cats.data.StateT
import org.slips.core.Macros
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.predicates.*
import org.slips.core.rule.Rule
import scala.language.implicitConversions
import scala.util.NotGiven

object syntax {
  inline def all[T: FactOps](using NotGiven[T <:< Tuple]): Condition.Source[T] = Condition.all[T]

  inline implicit def predicateToCondition(
    p: Predicate
  ): Condition[Unit] =
    Condition.OpaquePredicate(p)

  inline implicit def tupleToFact[T <: NonEmptyTuple, Q <: NonEmptyTuple](
    x: T
  )(using
    ev0: Q =:= Fact.TInverseMap[T],
    ev: T =:= Fact.TMap[Q],
    ev1: FactOps.TupleOps[Q]
  ): Fact[Q] =
    Fact.fromTuple(ev(x))

  inline implicit def liftToLiteralFact[T : Fact.CanBeLiteral : FactOps](
    x: T
  ): Fact[T] = Fact.literal(x)

  extension [T](
    fact: Fact[T]
  ) {
    inline def value[I: FactOps](
      inline f: T => I
    ): Fact[I] =
      Macros.createSigned[Fact.Map[T, I]](
        s => Fact.Map(s"${ fact.signature } => $s", f, fact),
        f
      )
  }

  def notExists[T](
    f: Fact.Val[T] => Predicate
  )(using F: FactOps[T]
  ): Condition[Unit] =
    F.allCondition.flatMap(v => Condition.OpaquePredicate(f(v)))

  extension [T: FactOps](
    c: Condition[T]
  )
    def makeRule(
      name: String
    ): Rule.Builder[T] =
      new Rule.Builder(name, c)

  def addFact[Q, T: NotTuple](
    t: T
  )(using env: Environment
  )(using r: env.Rule[Q]
  ): r.Action[Unit] =
    StateT(_.addFact(t))
}
