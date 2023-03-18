package org.slips.core.build

import org.slips.Env
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM

private[slips] case class ParseResult(
  rule: RuleM,
  sources: Set[Condition.Source[_]],
  predicates: BetaPredicates,
  predicatesAndSources: SelectedPredicatesAndSources
) {
  def predicateRules: PredicateRules = ???
}

private[slips] object ParseResult {

  def fromRule(
    rule: RuleM
  ): Env[ParseResult] = {
    val sp = rule.sourcesAndPredicates

    val pc = sp.predicates.values.flatten.foldLeft(ParseResult.empty)(_ addPredicate _)
    pc.toParseResult(rule, sp)
  }

}
