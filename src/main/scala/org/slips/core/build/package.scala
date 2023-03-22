package org.slips.core

import cats.compat.SortedSet
import cats.data.State
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM

package object build {

  type AlphaFacts      = Map[Fact.Source, Set[Predicate]]
  type AllFacts        = Map[Fact[_], Set[Predicate]]
  type AlphaPredicates = Set[AlphaPredicate]
  type BetaPredicates  = Map[Predicate, Set[Fact[_]]]
  type PredicateRules  = Map[Predicate, Set[RuleM]]

  type BuildStep[x] = State[BuildContext, x]

  extension (a: AlphaPredicates) {
    def addAlpha(predicate: Predicate): AlphaPredicates = {
      val alphaPredicate = AlphaPredicate(predicate)
      alphaPredicate.fold(a)(a + _)
    }
  }

  extension (b: BetaPredicates) {
    def addBeta(predicate: Predicate): BetaPredicates = {
      b + (predicate -> (b.getOrElse(predicate, Set.empty) ++ predicate.facts))
    }
  }

}
