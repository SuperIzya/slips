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
import org.slips.core.fact.FactOps.TupleOps
import org.slips.core.predicates.*
import org.slips.core.rule.Rule
import scala.annotation.targetName
import scala.language.implicitConversions
import scala.util.NotGiven

object syntax {

  type SimpleTuple2[T] = Fact.Val[T *: T *: EmptyTuple] =:= Fact[T] *: Fact[T] *: EmptyTuple

  inline def all[T : FactOps : NotTuple]: Condition.Source[T] = Condition.all[T]

  inline implicit def predicateToCondition(p: Predicate): Condition[Unit] = Condition.OpaquePredicate(p)

  inline implicit def liftToLiteralFact[T : Fact.CanBeLiteral : FactOps](x: T): Fact[T] = Fact.literal(x)

  extension [T](fact: Fact[T]) {
    inline def test(inline f: T => Boolean)(using T: FactOps[T], ev: Fact[T] =:= Fact.Val[T]): Signed[Predicate] = {
      Signed(f)(Predicate.Test(_, f, ev(fact)))
    }
    inline def value[I](inline f: T => I): Signed[Fact[I]]                                                       = {
      if (fact.isAlpha) Signed(f) { Fact.Alpha.Map(fact.asInstanceOf[Fact.Alpha[T]], f, _) }
      else ???
    }

    private inline def buildPredicate(other: Fact[T], inline test: (T, T) => Boolean)(using
      T: FactOps[T],
      ev: SimpleTuple2[T]
    ): Signed[Predicate] = {
      (fact, other) match {
        // case (a: Fact.Alpha[T], b: Fact.Alpha[T]) if a.alphaSources == b.alphaSources =>

        case (l: Fact.Literal[T], b: Fact[T]) =>
          Signed(test) {
            Predicate.Test(
              _,
              test(l.value, _),
              b
            )
          }
        case (a: Fact[T], l: Fact.Literal[T]) =>
          Signed(test) {
            Predicate.Test(
              _,
              test(_, l.value),
              a
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
      TT: TupleOps[T *: T *: EmptyTuple],
      T: Eq[T],
      ev: SimpleTuple2[T]
    ): Signed[Predicate] =
      buildPredicate(other, T.neqv)

    @targetName("repEq")
    inline def ===(other: Fact[T])(using
      TO: TupleOps[T *: T *: EmptyTuple],
      T: Eq[T],
      ev: SimpleTuple2[T]
    ): Signed[Predicate] =
      buildPredicate(other, T.eqv)

  }

  def notExists[T](f: Fact.Val[T] => Predicate)(using F: FactOps[T]): Condition[Unit] = ???

  extension [T: FactOps](c: Condition[T])
    def makeRule(name: String): Rule.Builder[T] =
      new Rule.Builder(name, c)

  def addFact[Q, T: NotTuple](t: T)(using env: Environment)(using r: env.Rule[Q]): r.Action[Unit] =
    StateT(_.addFact(t))
}
