package org.slips.core.build

import org.slips.core.conditions.Condition
import org.slips.core.fact.{Fact, FactOps}
import org.slips.core.predicates.Predicate

case class SelectedPredicatesAndSources(
  predicates: PredicateSelectionMap,
  sources: Set[Condition.Source[_]],
  facts: Set[Fact.Source[_]],
  discarded: Set[Predicate]
) {

  import SelectedPredicatesAndSources._

  def addPredicate(p: Predicate): PredicateSelectionMap = {
    addToMap(predicates, p)
  }

  def withPredicate(p: Predicate): SelectedPredicatesAndSources = copy(
    predicates = addPredicate(p),
    facts = facts ++ p.sourceFacts
  )

  def withDiscard(p: Predicate): SelectedPredicatesAndSources = copy(discarded = discarded + p)
}

object SelectedPredicatesAndSources {
  lazy val empty: SelectedPredicatesAndSources =
    SelectedPredicatesAndSources(
      predicates = Map.empty,
      sources = Set.empty,
      facts = Set.empty,
      discarded = Set.empty
    )

  private inline def addToMap(map: PredicateSelectionMap, p: Predicate): PredicateSelectionMap = {
    map ++ p.facts.flatMap(f => f.sourceFacts).map { f => f -> (map.getOrElse(f, Set.empty) + p) }
  }

  def apply[T](
    start: Fact.Val[T]
  )(using T: FactOps[T]
  ): SelectedPredicatesAndSources = {
    new SelectedPredicatesAndSources(
      predicates = Map.empty,
      sources = T.sources(start),
      facts = T.sourceFacts(start),
      discarded = Set.empty
    )
  }
}
