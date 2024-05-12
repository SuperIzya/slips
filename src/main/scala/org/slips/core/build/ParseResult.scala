package org.slips.core.build

import org.slips.{Env, Environment, Signature}
import org.slips.core.conditions.*
import org.slips.core.fact.{Fact, FactOps}
import org.slips.core.rule.Rule

import scala.annotation.showAsInfix

private[slips] case class ParseResult[F[_]](
                                       rule: Rule[F],
                                       sources: Set[String],
                                       allPredicates: AllPredicates,
                                       predicatesAndSources: SelectedPredicatesAndSources
) {
  def predicateRules: PredicateRules[F] = allPredicates.values.map(_.predicate)
    .map(_ -> Set(rule))
    .toMap
}

