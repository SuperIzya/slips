package org.slips.core

import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.TMap
import org.slips.core.fact.FactOps.TupleOps

package object fact {

  type Predecessors = List[Fact[_]]

  extension [T](fact: Fact[T]) {
    def toVal(using T: FactOps[T]): Fact.Val[T] = T.toVal(fact)
  }
  extension [T](fact: Fact.Val[T]) {
    def predecessors(using T: FactOps[T]): Predecessors        = T.predecessors(fact)
    def sources(using T: FactOps[T]): Set[Condition.Source[_]] = T.sources(fact)
    def signature(using T: FactOps[T]): String                 = T.extract(fact).mkString("(", ", ", ")")
    def facts(using T: FactOps[T]): Set[Fact[_]]               = T.facts(fact)
  }

  extension [T <: NonEmptyTuple](facts: TMap[T]) {
    def predecessors(using T: TupleOps[T]): Predecessors        = T.predecessors(T.toVal(facts))
    def sources(using T: TupleOps[T]): Set[Condition.Source[_]] = T.sources(T.toVal(facts))

    def signature(using T: TupleOps[T]): String = T.extract(T.toVal(facts)).mkString("(", ", ", ")")

    def toVal(using T: TupleOps[T]): Fact.Val[T] = T.toVal(facts)

    def facts(using T: TupleOps[T]): Set[Fact[_]] = T.facts(T.toVal(facts))
  }
}
