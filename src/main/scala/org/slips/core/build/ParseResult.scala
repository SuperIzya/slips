package org.slips.core.build

import org.slips.Env
import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.rule.Rule.RuleM
import scala.annotation.showAsInfix

private[slips] case class ParseResult(
  rule: RuleM,
  sources: Set[String],
  alphaPredicates: AlphaPredicates,
  betaPredicates: BetaPredicates,
  predicatesAndSources: SelectedPredicatesAndSources
) {
  def predicateRules: PredicateRules = (betaPredicates.keys ++ alphaPredicates.values.map(_.predicate))
    .map(_ -> Set(rule))
    .toMap
}

private[slips] object ParseResult {

  def fromRule(rule: RuleM): Env[ParseResult] = {
    val sp = rule.sourcesAndPredicates

    val pc = sp.predicates.keys.foldLeft(ParseCollector.empty)(_.addPredicate(_))
    pc.toParseResult(rule, sp)
  }

  private case class ParseCollector(
    alphaPredicates: AlphaPredicates = Map.empty,
    betaPredicates: BetaPredicates = Map.empty
  )

  private object ParseCollector {
    val empty: ParseCollector = ParseCollector()

    extension (collector: ParseCollector) {
      @showAsInfix
      def addPredicate(p: Predicate): Env[ParseCollector] = {
        collector.copy(
          alphaPredicates = collector.alphaPredicates.addAlpha(p),
          betaPredicates = collector.betaPredicates.addBeta(p)
        )
      }

      def toParseResult(ruleM: RuleM, ps: SelectedPredicatesAndSources): Env[ParseResult] = env ?=> {
        ParseResult(
          rule = ruleM,
          alphaPredicates = collector.alphaPredicates,
          betaPredicates = collector.betaPredicates,
          sources = ps.sources.map(env.signatureStrategy(_)),
          predicatesAndSources = ps
        )
      }
    }

  }
}
