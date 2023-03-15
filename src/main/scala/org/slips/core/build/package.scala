package org.slips.core

import cats.compat.SortedSet
import cats.data.State
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM

package object build {

  type SourcePredicates = Map[Fact.Source, Set[Predicate]]
  type PredicateMap[T]  = Map[Predicate, Set[T]]
  type AlphaPredicates  = Map[Predicate.AlphaTest[_], Set[Fact.Source]]
  type BetaPredicates   = Map[Predicate, Set[Fact.Source]]
  type PredicateRules   = PredicateMap[RuleM]

  type BuildStep[x] = State[BuildContext, x]

}
