package org.slips.core

import org.slips.Signature
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.TMap
import org.slips.core.fact.FactOps.ScalarFact

package object fact {

  type Predecessors = List[Fact[?]]

  extension [T](fact: Fact[T]) {
    def toVal(using T: FactOps[T], ev: ScalarFact[T]): Fact.Val[T] = ev.flip(fact)
  }

  extension [T](fact: Fact.Val[T]) {
    def predecessors(using T: FactOps[T]): Predecessors = T.predecessors(fact)
    def sources(using T: FactOps[T]): Set[Signature]    = T.sources(fact)
    def signature(using T: FactOps[T]): Signature       = Signature.Manual(T.extract(fact).mkString("(", ", ", ")"))
    def facts(using T: FactOps[T]): Set[Fact[?]]        = T.facts(fact)
  }

}
