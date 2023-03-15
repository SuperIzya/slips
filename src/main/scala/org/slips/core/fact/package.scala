package org.slips.core

import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.Predecessors
import org.slips.core.fact.Fact.TMap
import org.slips.core.fact.FactOps.TupleOps

package object fact {

  extension [T](fact: Fact[T]) {
    def toVal(using T: FactOps[T]): Fact.Val[T] = T.toVal(fact)
  }
  extension [T](fact: Fact.Val[T]) {
    def predecessors(using T: FactOps[T]): Predecessors        = T.predecessors(fact)
    def sources(using T: FactOps[T]): Set[Condition.Source[_]] = T.sources(fact)
    def signature(using T: FactOps[T]): String                 = T.extract(fact).mkString("(", ", ", ")")

    def toFacts(using T: FactOps[T]): Set[Fact[_]] = fact match
      case f: TMap[_] => f.toList.iterator.map(_.asInstanceOf[Fact[_]]).iterator.to(Set)
      case x: Fact[_] => Set(x)
  }

  extension [T <: NonEmptyTuple](facts: TMap[T]) {
    def predecessors(using T: TupleOps[T]): Predecessors        = T.predecessors(T.toVal(facts))
    def sources(using T: TupleOps[T]): Set[Condition.Source[_]] = T.sources(T.toVal(facts))

    def signature(using T: TupleOps[T]): String = T.extract(T.toVal(facts)).mkString("(", ", ", ")")

    def toVal(using T: TupleOps[T]): Fact.Val[T] = T.toVal(facts)
  }
}
