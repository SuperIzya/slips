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
  def predicateRules: PredicateRules = predicatesAndSources
    .alphaFacts
    .values
    .foldLeft(Set.empty[Predicate])(_ ++ _)
    .map(_ -> Set(rule))
    .toMap
}

private[slips] object ParseResult {

  def fromRule(
    rule: RuleM
  ): Env[ParseResult] = {
    val sp = rule.sourcesAndPredicates

    val pc = sp.alphaFacts.values.flatten.foldLeft(ParseResult.empty)(_ addPredicate _)
    pc.toParseResult(rule, sp)
  }

  private case class ParseCollector(alpha: AlphaPredicates, beta: BetaPredicates)
  private val empty: ParseCollector = ParseCollector(Map.empty, Map.empty)

  private object ParseCollector {

    extension (m: AlphaPredicates) {
      def getM(p: Predicate.AlphaTest[_]): Set[Fact.Source] = m.getOrElse(p, Set.empty) ++ p.sourceFacts
    }

    extension (m: BetaPredicates) {
      def getM(p: Predicate): Set[Fact[_]] = m.getOrElse(p, Set.empty) ++ p.sourceFacts
    }
    extension (collector: ParseCollector) {
      def addPredicate(p: Predicate): ParseCollector = p match {
        case pa: Predicate.AlphaTest[_] =>
          collector.copy(alpha = collector.alpha + (pa -> collector.alpha.getM(pa)))
        case _                          =>
          collector.copy(beta = collector.beta + (p -> collector.beta.getM(p)))
      }

      def toParseResult(ruleM: RuleM, ps: SelectedPredicatesAndSources): ParseResult = ParseResult(
        rule = ruleM,
        alphaPredicates = collector.alpha,
        betaPredicates = collector.beta,
        sources = ps.sources,
        predicatesAndSources = ps
      )
    }

  }
}
