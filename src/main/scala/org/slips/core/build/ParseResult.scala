package org.slips.core.build

import org.slips.Env
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM

private[slips] case class ParseResult(
  rule: RuleM,
  sources: Set[Condition.Source[_]],
  alphaPredicates: AlphaPredicates,
  betaPredicates: BetaPredicates,
  predicatesAndSources: SelectedPredicatesAndSources
) {
  def predicateRules: PredicateRules =
    (alphaPredicates.map(_.asInstanceOf[Predicate] -> rule) ++ betaPredicates.keys.map(_ -> rule)).toMap
}

private[slips] object ParseResult {

  def fromRule(
    rule: RuleM
  ): Env[ParseResult] = {
    val sp = rule.sourcesAndPredicates

    val pc = sp.predicates.keys.foldLeft(ParseCollector.empty)(_ addPredicate _)
    pc.toParseResult(rule, sp)
  }

  private case class ParseCollector(
    alphaPredicates: AlphaPredicates = Set.empty,
    betaPredicates: BetaPredicates = Map.empty
  )

  private object ParseCollector {
    val empty: ParseCollector = ParseCollector()

    extension (collector: ParseCollector) {
      def addPredicate(p: Predicate): ParseCollector = {
        collector.copy(
          alphaPredicates = collector.alphaPredicates.addAlpha(p),
          betaPredicates = collector.betaPredicates.addBeta(p)
        )
      }

      def toParseResult(ruleM: RuleM, ps: SelectedPredicatesAndSources): ParseResult = ParseResult(
        rule = ruleM,
        alphaPredicates = collector.alphaPredicates,
        betaPredicates = collector.betaPredicates,
        sources = ps.sources,
        predicatesAndSources = ps
      )
    }

  }
}
