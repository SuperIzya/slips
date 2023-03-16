package org.slips.core

import cats.compat.SortedSet
import cats.data.State
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM

package object build {

  type AlphaFacts      = Map[Fact.Source, Set[Predicate.Alpha]]
  type BetaFacts       = Map[Fact[_], Set[Predicate.Beta]]
  type PredicateMap[T] = Map[Predicate, Set[T]]
  type AlphaPredicates = Map[Predicate.AlphaTest[_], Set[Fact.Source]]
  type BetaPredicates  = Map[Predicate, Set[Fact[_]]]
  type PredicateRules  = PredicateMap[RuleM]

  type BuildStep[x] = State[BuildContext, x]

}
