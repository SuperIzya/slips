package org.slips.core.build

import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.predicates.Predicate

case class SelectedPredicatesAndSources(
  predicates: SourcePredicates,
  sources: Set[Condition.Source[_]],
  facts: Set[Fact.Source],
  discarded: Set[Predicate]
) {

  import SelectedPredicatesAndSources._

  def addPredicate(
    p: Predicate
  ): SourcePredicates = addToMap(predicates, p)

  def withPredicate(
    p: Predicate
  ): SelectedPredicatesAndSources = copy(
    predicates = addPredicate(p),
    facts = facts ++ p.sourceFacts
  )

  def withDiscard(
    p: Predicate
  ): SelectedPredicatesAndSources = copy(discarded = discarded + p)
}

object SelectedPredicatesAndSources {
  lazy val empty: SelectedPredicatesAndSources =
    SelectedPredicatesAndSources(
      predicates = Map.empty,
      sources = Set.empty,
      facts = Set.empty,
      discarded = Set.empty
    )

  private inline def addToMap(
    map: SourcePredicates,
    p: Predicate
  ): SourcePredicates = {
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
