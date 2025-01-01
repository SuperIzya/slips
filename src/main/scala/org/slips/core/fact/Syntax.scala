package org.slips.core.fact

import cats.{Eq, Order}
import org.slips.Signature
import org.slips.core.SourceLocation
import org.slips.core.conditions.Predicate
import org.slips.core.fact.*
import org.slips.core.fact.Fact.CanBeLiteral
import org.slips.core.fact.FactOps.TupleOps

import scala.compiletime.summonInline

trait Syntax {
  implicit def literal[T: {CanBeLiteral, FactOps, ScalarFact}](v: T)(using SourceLocation): Fact[T] =
    new Fact.Literal(v)

  extension [T, Q](fact: Fact.Map[T, Q]) {
    def signed(signature: String)(using Q: FactOps[Q]): Fact.Map[T, Q] =
      signed(Signature.Manual(signature))

    def signed(signature: Signature)(using Q: FactOps[Q]): Fact.Map[T, Q] = {
      given SourceLocation = fact.sourceLocation

      fact.copy(mapSign = signature)
    }
  }

  extension [T: {FactOps, ScalarFact}](fact: Fact[T]) {
    inline def value[I: {FactOps, ScalarFact}](inline f: T => I)(using SourceLocation): Fact[I] =
      Fact.Map(fact, f, Signature.auto(f).unite(fact)((s, f) => s"$f => $s"))

    inline def test(inline f: T => Boolean)(using SourceLocation): Predicate =
      Predicate.Test[T](
        signature = Signature.auto(f).unite(fact)((s, f) => s"$f ? (1)$s"),
        test = f,
        rep = summonInline[ScalarFact[T]].flip(fact)
      )

    inline def matches(inline f: PartialFunction[T, Boolean])(using SourceLocation): Predicate =
      Predicate.Test[T](
        signature = Signature.auto(f).unite(fact)((s, f) => s"$f matches (1)$s"),
        rep = summonInline[ScalarFact[T]].flip(fact),
        test = x => f.isDefinedAt(x) && f(x)
      )

    def =!=(other: Fact[T])(using eq: Eq[T], sl: SourceLocation): Predicate = testTwo(fact, other)(!eq.eqv(_, _))
    def ===(other: Fact[T])(using eq: Eq[T], sl: SourceLocation): Predicate = testTwo(fact, other)(eq.eqv)
    def <(other: Fact[T])(using ord: Order[T], sl: SourceLocation): Predicate = testTwo(fact, other)(ord.lt)
    def >(other: Fact[T])(using ord: Order[T], sl: SourceLocation): Predicate = testTwo(fact, other)(ord.gt)
    def <=(other: Fact[T])(using ord: Order[T], sl: SourceLocation): Predicate = testTwo(fact, other)(ord.lteqv)
    def >=(other: Fact[T])(using ord: Order[T], sl: SourceLocation): Predicate = testTwo(fact, other)(ord.gteqv)

  }

  private inline def testTwo[T: {FactOps, ScalarFact}](left: Fact[T], right: Fact[T])(
    inline f: (T, T) => Boolean
  )(using TupleOps[(T, T)], SourceLocation): Predicate = {
    val arity = Set(left.source, right.source)
    val sign = left.signature.unite(right)((a, b) => s"($a, $b)").unite(Signature.auto(f))(_ + s" ? ($arity)" + _)
    val test: ((T, T)) => Boolean = f.tupled
    Predicate.Test[(T, T)](
      signature = sign,
      test = test,
      rep = (left, right)
    )
  }

  extension [T <: NonEmptyTuple](fact: T) {
    inline def testMany[Q <: NonEmptyTuple](
                                             inline f: Q => Boolean
                                           )(
                                             using Q: TupleOps[Q],
                                             ev: TupleFact[Q],
                                             ev2: Q =:= Fact.InverseVal[T],
                                             ev3: T =:= Fact.Val[Q],
                                             sl: SourceLocation
                                           ): Predicate = {
      val arity = Q.sources(ev3(fact)).size
      Predicate.Test[Q](
        signature = Q.signature.unite(Signature.auto(f))(_ + s" ? ($arity)" + _),
        test = f,
        rep = ev3(fact)
      )
    }

    inline def matchesMany[Q <: NonEmptyTuple](
                                                inline f: PartialFunction[Q, Boolean]
                                              )(
                                                using Q: TupleOps[Q],
                                                ev: TupleFact[Q],
                                                ev2: Q =:= Fact.InverseVal[T],
                                                ev3: T =:= Fact.Val[Q],
                                                sl: SourceLocation
                                              ): Predicate = {
      val arity = Q.sources(ev3(fact)).size
      Predicate.Test[Q](
        signature = Q.signature.unite(Signature.auto(f))(_ + s" matches ($arity)" + _),
        rep = ev3(fact),
        test = x => f.isDefinedAt(x) && f(x)
      )
    }

  }
}
