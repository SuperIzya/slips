package org.slips.syntax

import org.slips.Signature
import org.slips.core.Macros
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.CanBeLiteral
import org.slips.core.fact.FactOps.TupleOps
import org.slips.core.predicates.Predicate
import scala.annotation.targetName
import scala.language.implicitConversions

trait FactSyntax {

  implicit def literal[T : CanBeLiteral : FactOps](v: T): Fact[T] = new Fact.Literal(v)

  extension [T](fact: Fact[T]) {
    inline def value[I: FactOps](inline f: T => I): Fact[I] =
      Macros.createSigned[Fact.Map[T, I]](
        s => Fact.Map(Signature.derivedUnary(fact, f => s"$f => $s"), f, fact),
        f
      )

    inline def test(inline f: T => Boolean)(using ev: ScalarFact[T], T: FactOps[T]): Predicate =
      Predicate.Test.fromFact(ev.flip(fact), f)

    @targetName("repNotEq")
    inline def =!=(other: Fact[T])(using ScalarFact[T], FactOps[T]): Predicate =
      Predicate.Test[T](fact, other, _ != _)

    @targetName("repEq")
    inline def ===(other: Fact[T])(using ScalarFact[T], FactOps[T]): Predicate =
      Predicate.Test[T](fact, other, _ == _)

  }

  extension [T <: NonEmptyTuple](fact: T) {
    inline def testMany[Q <: NonEmptyTuple](
      inline f: Fact.InverseVal[T] => Boolean
    )(using
      ev2: Fact.InverseVal[T] =:= Q,
      Q: TupleOps[Q],
      ev: TupleFact[Q],
      ev3: T =:= Fact.Val[Q]
    ): Predicate =
      Predicate.Test.fromTuple[Q](ev3(fact), ev2.flip.andThen(f)) // TODO: Try REPL, (f -> h).testMany(_ => true)

  }
}
