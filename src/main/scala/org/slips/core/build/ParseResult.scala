package org.slips.core.build

import org.slips.Env
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
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

private[slips] object ParseResult {

  def fromRule(
    rule: RuleM
  ): Env[ParseResult] = {
    val sp = rule.sourcesAndPredicates

    val pc = sp.predicates.values.flatten.foldLeft(ParseResult.empty)(_ addPredicate _)
    pc.toParseResult(rule, sp)
  }

  case class ParseCollector(
    alpha: PredicateMap,
    beta: PredicateMap,
    gamma: PredicateMap
  )
  private val empty: ParseCollector = ParseCollector(Map.empty, Map.empty, Map.empty)

  object ParseCollector {

    extension (
      m: PredicateMap
    )
      def getM(
        p: Predicate
      ): Set[Fact.Source[_]] = m.getOrElse(p, Set.empty) ++ p.sourceFacts

    extension (
      collector: ParseCollector
    ) {
      def addPredicate(
        p: Predicate
      ): ParseCollector = {
        if (p.sourceFacts.size == 1)
          collector.copy(alpha = collector.alpha + (p -> collector.alpha.getM(p)))
        else if (p.sourceFacts.size == 2)
          collector.copy(beta = collector.beta + (p -> collector.beta.getM(p)))
        else collector.copy(gamma = collector.gamma + (p -> collector.gamma.getM(p)))
      }
      def toParseResult(
        ruleM: RuleM,
        ps: SelectedPredicatesAndSources
      ): ParseResult = ParseResult(
        rule = ruleM,
        alphaPredicates = collector.alpha,
        betaPredicates = collector.beta,
        gammaPredicates = collector.gamma,
        sources = ps.sources,
        predicatesAndSources = ps
      )
    }

  }
}
