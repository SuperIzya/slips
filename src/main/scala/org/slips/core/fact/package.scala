package org.slips.core

import org.slips.Signature
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.TMap
import org.slips.core.fact.FactOps.TupleOps

package object fact {

  type Predecessors = List[Fact[_]]

  extension [T](fact: Fact[T]) {
    def toVal(using T: FactOps[T]): Fact.Val[T] = T.splitToFacts(fact)
  }

  extension [T](fact: Fact.Val[T]) {
    def predecessors(using T: FactOps[T]): Predecessors = T.predecessors(fact)
    def sources(using T: FactOps[T]): Set[Signature]    = T.sources(fact)
    def signature(using T: FactOps[T]): Signature       = Signature.Manual(T.extract(fact).mkString("(", ", ", ")"))
    def facts(using T: FactOps[T]): Set[Fact[_]]        = T.facts(fact)
  }

  extension [T <: NonEmptyTuple](facts: TMap[T]) {
    def predecessors(using T: TupleOps[T], ev: TMap[T] =:= Fact.Val[T]): Predecessors = T.predecessors(ev(facts))
    def sources(using T: TupleOps[T], ev: TMap[T] =:= Fact.Val[T]): Set[Signature]    = T.sources(ev(facts))

    def signature(using T: TupleOps[T], ev: TMap[T] =:= Fact.Val[T]): Signature = Signature.Manual {
      T.extract(ev(facts)).mkString("(", ", ", ")")
    }

    def toVal(using ev: TMap[T] =:= Fact.Val[T]): Fact.Val[T] = ev(facts)

    def facts(using T: TupleOps[T], ev: TMap[T] =:= Fact.Val[T]): Set[Fact[_]] = T.facts(ev(facts))
  }
}
