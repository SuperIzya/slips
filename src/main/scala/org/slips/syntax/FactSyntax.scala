package org.slips.syntax

import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.fact.Fact.CanBeLiteral
import org.slips.core.fact.FactOps.TupleOps
import scala.annotation.targetName
import scala.compiletime.summonInline
import scala.language.implicitConversions

trait FactSyntax {

  implicit def literal[T : CanBeLiteral : FactOps](v: T): Fact[T] = new Fact.Literal(v)

  extension [T, Q](fact: Fact.Alpha.Map[T, Q]) {
    def signed(signature: String)(using Q: FactOps[Q]): Fact.Alpha.Map[T, Q] =
      signed(Signature.Manual(signature))

    def signed(signature: Signature)(using Q: FactOps[Q]): Fact.Alpha.Map[T, Q] =
      fact.copy(mapSign = signature)
  }

  extension [T : FactOps : ScalarFact](fact: Fact[T]) {
    inline def value[I : FactOps : ScalarFact](inline f: T => I): Fact[I] =
      Fact.Map(
        signature = Signature.auto(f).unite(fact)((s, f) => s"$f => $s"),
        f = f,
        rep = fact
      )

    inline def test(inline f: T => Boolean): Predicate =
      Predicate.Test[T](
        signature = Signature.auto(f).unite(fact)((s, f) => s"$f -> $s"),
        test = f,
        rep = summonInline[ScalarFact[T]].flip(fact)
      )

    @targetName("repNotEq")
    def =!=(other: Fact[T]): Predicate =
      testTwo(fact, other, _ != _)

    @targetName("repEq")
    def ===(other: Fact[T]): Predicate =
      testTwo(fact, other, _ == _)

    private inline def testTwo(left: Fact[T], right: Fact[T], inline f: (T, T) => Boolean): Predicate =
      Predicate.Test[(T, T)](
        signature = left.signature * Signature.auto(f) * right,
        test = f.tupled,
        rep = (left, right)
      )

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
      Predicate.Test[Q](
        signature = Q.signature.unite(Signature.auto(f))(_ + " -> " + _),
        test = ev2.flip.andThen(f),
        rep = ev3(fact)
      )

  }
}
