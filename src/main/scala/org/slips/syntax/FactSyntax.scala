package org.slips.syntax

import cats.Eq
import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.fact.Fact.CanBeLiteral
import org.slips.core.fact.FactOps.TupleOps

import scala.compiletime.summonInline
import scala.language.implicitConversions

trait FactSyntax {

  implicit def literal[T: CanBeLiteral : FactOps: ScalarFact](v: T): Fact[T] = new Fact.Literal(v)

  extension [T, Q](fact: Fact.Map[T, Q]) {
    def signed(signature: String)(using Q: FactOps[Q]): Fact.Map[T, Q] =
      signed(Signature.Manual(signature))

    def signed(signature: Signature)(using Q: FactOps[Q]): Fact.Map[T, Q] =
      fact.copy(mapSign = signature)
  }
  
  extension [T: FactOps : ScalarFact](fact: Fact[T]) {
    inline def value[I: FactOps : ScalarFact](inline f: T => I): Fact[I] =
      Fact.Map(fact, f, Signature.auto(f).unite(fact)((s, f) => s"$f => $s"))

    inline def test(inline f: T => Boolean): Predicate =
      Predicate.Test[T](
        signature = Signature.auto(f).unite(fact)((s, f) => s"$f ? (1)$s"),
        test = f,
        rep = summonInline[ScalarFact[T]].flip(fact)
      )

    inline def matches[Q](inline f: T => Option[Q]): Predicate = ???

    inline def !=(other: Fact[T])(using eq: Eq[T]): Predicate = testTwo(fact, other)(!eq.eqv(_, _))
    inline def ==(other: Fact[T])(using eq: Eq[T]): Predicate = testTwo(fact, other)(eq.eqv)
    inline def <(other: Fact[T])(using le: cats.Order[T]): Predicate = testTwo(fact, other)(le.lt)
    inline def >(other: Fact[T])(using le: cats.Order[T]): Predicate = testTwo(fact, other)(le.gt)
    inline def <=(other: Fact[T])(using le: cats.Order[T]): Predicate = testTwo(fact, other)(le.lteqv)
    inline def >=(other: Fact[T])(using le: cats.Order[T]): Predicate = testTwo(fact, other)(le.gteqv)

  }

  private inline def testTwo[T: FactOps : ScalarFact](left: Fact[T], right: Fact[T])
                                                     (inline f: (T, T) => Boolean)
                                                     (using TupleOps[(T, T)]): Predicate = {
    val arity = Set(left.source, right.source)
    val sign = left.signature.unite(right)((a, b) => s"($a, $b)").unite(Signature.auto(f))(_ + s" ? ($arity)" + _)
    Predicate.Test[(T, T)](
      signature = sign,
      test = f.tupled,
      rep = (left, right)
    )
  }

  extension [T <: NonEmptyTuple](fact: T) {
    inline def testMany[Q <: NonEmptyTuple](
                                             inline f: Q => Boolean
                                           )(using
                                             Q: TupleOps[Q],
                                             ev: TupleFact[Q],
                                             ev2: Q =:= Fact.InverseVal[T],
                                             ev3: T =:= Fact.Val[Q]
                                           ): Predicate = {
      val arity = ev3(fact).sources.size
      Predicate.Test[Q](
        signature = Q.signature.unite(Signature.auto(f))(_ + s" ? ($arity)" + _),
        test = f,
        rep = ev3(fact)
      )
    }

  }
}

