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

  extension [T](fact: Fact[T]) {
    inline def value[I: FactOps](inline f: T => I): Fact[I] =
      Macros.createSigned[Fact.Map[T, I]](
        s => Fact.Map(Signature.derivedUnary(fact, f => s"$f => $s"), f, fact),
        f
      )

    inline def test(inline f: T => Boolean)(using ev: ScalarFact[T], T: FactOps[T]): Predicate =
      Predicate.Test.fromFact(ev.flip(fact), f)

    @targetName("repNotEq")
    inline def =!=(other: Fact[T])(using ScalarFact[T], FactOps[T]): Predicate = ???
    // TODO: Predicate.Test[T](fact, other, _ != _)

    @targetName("repEq")
    inline def ===(other: Fact[T])(using ScalarFact[T], FactOps[T]): Predicate = ???
    // TODO: Predicate.Test[T](fact, other, _ == _)

  }

  implicit def toLiteralFact[T : CanBeLiteral : FactOps](t: T): Fact[T] = Fact.literal(t)

  extension [T <: NonEmptyTuple](fact: Fact.Val[T]) {
    inline def testMany(inline f: T => Boolean)(using T: TupleOps[T], ev: TupleFact[T]): Predicate =
      Predicate.Test.fromTuple[T](fact, f) // TODO: Try REPL, (f -> h).testMany(_ => true)

  }
}
