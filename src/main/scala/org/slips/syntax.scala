package org.slips

import cats.Eq
import cats.data.StateT
import org.slips.core.Macros
import org.slips.core.SignatureStrategy
import org.slips.core.Signed
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.fact.FactOps.TupleOps
import org.slips.core.predicates.*
import org.slips.core.rule.Rule
import scala.annotation.targetName
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

  extension [T](fact: Fact[T]) {
    inline def test(inline f: T => Boolean)(using T: FactOps[T]): Signed[Predicate]        = {
      Signed.createObject(Predicate.Test(_, f, T.toVal(fact)), f)
    }
    inline def value[I](inline f: T => I): Signed[Fact[I]]                                 = {
      if (fact.isAlpha) Signed.createObject(Fact.Alpha.Map(fact.asInstanceOf[Fact.Alpha[T]], f, _), f)
      else ???
    }

    private def buildPredicate(other: Fact[T], test: (T, T) => Boolean)(using
      TT: TupleOps[T *: T *: EmptyTuple]
    ): Predicate = {
      (fact.isAlpha, other.isAlpha) match {
        case (true, true) if other.alphaSources == fact.alphaSources =>
          Predicate.Test(
            test.hashCode().toString,
            test.tupled,
            TT.toVal(Fact.Alpha.Same(sourceFact = fact.alphaSources.head, collected = TT.toVal(fact -> other)))
          )
        case _                                                       => ???
      }
    }
    @targetName("repNotEq")
    def =!=(other: Fact[T])(using TT: TupleOps[T *: T *: EmptyTuple], T: Eq[T]): Predicate =
      buildPredicate(other, T.neqv)

    @targetName("repEq")
    inline def ===(other: Fact[T])(using TO: TupleOps[T *: T *: EmptyTuple], T: Eq[T]): Predicate =
      buildPredicate(other, T.eqv)

  }

  def notExists[T](f: Fact.Val[T] => Predicate)(using F: FactOps[T]): Condition[Unit] = ???

  extension [T: FactOps](c: Condition[T])
    def makeRule(name: String): Rule.Builder[T] =
      new Rule.Builder(name, c)

  def addFact[Q, T: NotTuple](t: T)(using env: Environment)(using r: env.Rule[Q]): r.Action[Unit] =
    StateT(_.addFact(t))
}
