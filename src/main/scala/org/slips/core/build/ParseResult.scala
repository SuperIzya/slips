package org.slips.core.build

import org.slips.core.conditions.Condition
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM

private[slips] case class ParseResult(
  rule: RuleM,
  sources: Set[Condition.Source[_]],
  alphaPredicates: PredicateMap,
  betaPredicates: PredicateMap,
  gammaPredicates: PredicateMap,
  predicatesAndSources: SelectedPredicatesAndSources
)
