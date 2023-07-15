package org.slips.core

import org.slips.Signature
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.TMap

package object fact {

  type Predecessors = List[Fact[?]]

  extension [T](fact: Fact[T]) {
    def toVal(using T: FactOps[T]): Fact.Val[T] = T.splitToFacts(fact)
  }

  extension [T](fact: Fact.Val[T]) {
    def predecessors(using T: FactOps[T]): Predecessors = T.predecessors(fact)
    def sources(using T: FactOps[T]): Set[Signature]    = T.sources(fact)
    def signature(using T: FactOps[T]): Signature       = Signature.Manual(T.extract(fact).mkString("(", ", ", ")"))
    def facts(using T: FactOps[T]): Set[Fact[?]]        = T.facts(fact)
  }

}
