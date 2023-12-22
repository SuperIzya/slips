package org.slips.syntax

import org.slips.Signature
import org.slips.core.Macros
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.FactOps.TupleOps
import org.slips.core.predicates.Predicate
import scala.annotation.targetName

trait FactSyntax {

  extension [T](fact: Fact[T]) {
    inline def value[I: FactOps](inline f: T => I): Fact[I] =
      Macros.createSigned[Fact.Map[T, I]](
        s => Fact.Map(Signature.derivedUnary(fact, f => s"$f => $s"), f, fact),
        f
      )

    inline def test(inline f: T => Boolean)(using ev: ScalarFact[T], T: FactOps[T]): Predicate =
      Predicate.Test.fromFact(ev.flip(fact), f)

    @targetName("repNotEq")
    inline def =!=(other: Fact[T])(using ScalarFact[T]): Predicate =
      Predicate.Test(fact, other, _ != _)

    @targetName("repEq")
    inline def ===(other: Fact[T])(using ScalarFact[T]): Predicate =
      Predicate.Test(fact, other, _ == _)

  }

  extension [T <: NonEmptyTuple](fact: Fact.TMap[T]) {
    inline def test(inline f: T => Boolean)(using T: TupleOps[T], ev: TupleFact[T]): Predicate =
      Predicate.Test.fromTuple[T](fact, f)
  }

}
