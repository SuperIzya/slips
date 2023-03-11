package org.slips.core

import cats.data.State
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM

package object build {

  type SourcePredicates = Map[Fact.Source[_], Set[Predicate]]
  type PredicateMap[T]  = Map[Predicate, Set[T]]
  type PredicateSources = PredicateMap[Fact.Source[_]]
  type PredicateRules   = PredicateMap[RuleM]

  type BuildStep[x] = State[BuildContext, x]

}
