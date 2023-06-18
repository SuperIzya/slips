package org.slips

import cats.Eq
import cats.data.StateT
import org.slips.core.Macros
import org.slips.core.SignatureStrategy
import org.slips.core.Signed
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.fact.Fact.CanBeLiteral
import org.slips.core.fact.Fact.Val
import org.slips.core.fact.FactOps.ScalarFact
import org.slips.core.predicates.*
import org.slips.core.rule.Rule
import scala.annotation.targetName
import scala.compiletime.summonInline
import scala.language.implicitConversions
import scala.util.NotGiven

object syntax {

  type SimpleTuple2[T] = Fact.Val[T *: T *: EmptyTuple] =:= Fact[T] *: Fact[T] *: EmptyTuple

  inline def all[T : FactOps : NotTuple]: Condition.Source[T] = Condition.all[T]

  inline implicit def predicateToCondition(p: Predicate): Condition[Unit] = Condition.OpaquePredicate(p)

  inline implicit def liftToLiteralFact[T : Fact.CanBeLiteral : FactOps](x: T): Fact[T] = Fact.literal(x)

  extension [T <: Tuple](fact: Fact.Val[T]) {
    inline def test2(inline f: T => Boolean)(using T: FactOps[T]): Predicate = {
      Signed(f) {
        Predicate.Test(
          _,
          f,
          fact
        )(using T)
      }
    }
  }

  extension [T](fact: Fact[T]) {
    inline def test(inline f: T => Boolean)(using T: FactOps[T], ev: ScalarFact[T]): Predicate = {
      Signed(f)(Predicate.Test(_, f, ev.flip(fact)))
    }

    inline def value[I](inline f: T => I): Fact[I] = {
      if (fact.isAlpha) Signed(f) {
        Fact.Alpha.Map(fact.asInstanceOf[Fact.Alpha[T]], f, _)
      }
      else ???
    }

    private inline def buildPredicate(other: Fact[T], inline test: (T, T) => Boolean)(using
      T: FactOps[T],
      ev: SimpleTuple2[T],
      ev2: Fact.Val[T] =:= Fact[T],
      TT: FactOps[T *: T *: EmptyTuple]
    ): Predicate = {
      (fact, other) match {
        // case (a: Fact.Alpha[T], b: Fact.Alpha[T]) if a.alphaSources == b.alphaSources =>

        case (l: Fact.Literal[T], b: Fact[T]) =>
          Signed(test) {
            Predicate.Test(
              _,
              test(l.value, _),
              ev2.flip(b)
            )
          }
        case (a: Fact[T], l: Fact.Literal[T]) =>
          Signed(test) {
            Predicate.Test(
              _,
              test(_, l.value),
              ev2.flip(a)
            )
          }
        case _                                =>
          Signed(test) {
            Predicate.Test(
              _,
              test.tupled,
              ev.flip(fact *: other *: EmptyTuple)
            )
          }
      }
    }

    @targetName("repNotEq")
    def =!=(other: Fact[T])(using
      TT: FactOps[(T, T)],
      F: FactOps[T],
      T: Eq[T],
      ev: SimpleTuple2[T],
      ev2: Fact.Val[T] =:= Fact[T]
    ): Predicate =
      buildPredicate(other, T.neqv)

    @targetName("repEq")
    inline def ===(other: Fact[T])(using
      TO: FactOps[(T, T)],
      F: FactOps[T],
      T: Eq[T],
      ev: SimpleTuple2[T],
      ev2: Fact.Val[T] =:= Fact[T]
    ): Predicate =
      buildPredicate(other, T.eqv)

  }

  def notExists[T](f: Fact.Val[T] => Predicate)(using F: FactOps[T]): Condition[Unit] = ???

  extension [T: FactOps](c: Condition[T])
    def makeRule(name: String): Rule.Builder[T] =
      new Rule.Builder(name, c)

  def addFact[Q, T: NotTuple](t: T)(using env: Environment)(using r: env.Rule[Q]): r.Action[Unit] =
    StateT(_.addFact(t))
}
