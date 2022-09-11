package org.slips.core

import cats.data.State
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate

package object build {

  type PredicateMap          = Map[Predicate, Set[Fact.Source[_]]]
  type PredicateSelectionMap = Map[Fact.Source[_], Set[Predicate]]

  type BuildStep[x] = State[BuildContext, x]

}
