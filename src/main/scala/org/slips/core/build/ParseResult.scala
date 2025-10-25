package org.slips.core.build

import org.slips.Env
import org.slips.Environment
import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.rule.Rule
import scala.annotation.showAsInfix

private[slips] final case class ParseResult[F[_]](
  rule: Rule[F],
  sources: Set[String],
  allPredicates: AllPredicates,
  allFactsChecks: Map[Fact[?], ParseResult.FactCheckList],
  predicatesAndSources: SelectedPredicatesAndSources
)

private[slips] object ParseResult {
  case class FactCheckList(fact: Fact[?], predicates: Set[String])
  object FactCheckList {
    def apply(fact: Fact[?], predicate: String): FactCheckList = FactCheckList(fact, Set(predicate))

    extension (factCheckList: FactCheckList) {
      def addPredicate(predicate: String): FactCheckList =
        factCheckList.copy(predicates = factCheckList.predicates + predicate)
    }
  }

  extension [F[_]](pr: ParseResult[F]) {
    def predicateRules: PredicateRules[F] = pr.allPredicates.values.map(_.predicate).map(_ -> Set(pr.rule)).toMap
  }
}
