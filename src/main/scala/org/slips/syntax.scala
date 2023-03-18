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
  inline def all[T : FactOps : NotTuple]: Condition.Source[T] = Condition.all[T]

  inline implicit def predicateToCondition(p: Predicate): Condition[Unit] = Condition.OpaquePredicate(p)

  inline implicit def tupleToFact[T <: NonEmptyTuple, Q <: NonEmptyTuple](x: T)(using
    ev: T =:= Fact.TMap[Q],
    Q: FactOps.TupleOps[Q]
  ): Fact.Val[Q] =
    Q.toVal(ev(x))

  inline implicit def liftToLiteralFact[T : Fact.CanBeLiteral : FactOps](x: T): Fact[T] = Fact.literal(x)

  extension [T](fact: Fact.Alpha[T]) {
    def value[I](f: T => I): Fact.Alpha[I]                               = Fact.Alpha.Map(fact, f)
    inline def test(inline f: T => Boolean)(using FactOps[T]): Predicate = Predicate.Test(fact, f)
  }

  extension [T](fact: Fact[T]) {
    inline def test(inline f: T => Boolean)(using FactOps[T]): Predicate = Predicate.Test(fact, f)
    inline def value(inline f: T => I): Fact[I]                          = {
      if (fact.isAlpha) fact.asInstanceOf[Fact.Alpha[T]].value(f)
      else ???
    }
  }

  def notExists[T](f: Fact.Val[T] => Predicate)(using F: FactOps[T]): Condition[Unit] = ???

  extension [T: FactOps](c: Condition[T])
    def makeRule(name: String): Rule.Builder[T] =
      new Rule.Builder(name, c)

  def addFact[Q, T: NotTuple](t: T)(using env: Environment)(using r: env.Rule[Q]): r.Action[Unit] =
    StateT(_.addFact(t))
}
