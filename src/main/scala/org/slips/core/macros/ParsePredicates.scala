package org.slips.core.macros

import org.slips.Signature
import org.slips.core.conditions.Predicate
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[slips] object ParsePredicates {
  inline def testM[T <: NonEmptyTuple](fact: Fact.Val[T], inline f: T => Boolean)(using T: FactOps[T]): Predicate =
    ${ parsePredicates('fact, 'f, 'T) }

  private def parsePredicates[T <: NonEmptyTuple](fact: Expr[Fact.Val[T]], f: Expr[T => Boolean], T: Expr[FactOps[T]])(
    using quotes: Quotes)(using tpeT: Type[T]): Expr[Predicate] = {
    println(f.show)
    '{
      Predicate.Test[T](
        signature = $T.signature.unite(Signature.auto($f))(_ + " -> " + _),
        test = $f,
        rep = $fact
      )(using $T)
    }
  }

}
