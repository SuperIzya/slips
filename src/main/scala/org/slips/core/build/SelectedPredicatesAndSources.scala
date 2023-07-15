package org.slips.core.build

import org.slips.Signature
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.predicates.Predicate

case class SelectedPredicatesAndSources(
  predicates: Map[Predicate, Set[Fact[?]]] = Map.empty,
  sources: Set[Signature] = Set.empty,
  alphaSources: Set[Fact.Source] = Set.empty,
  facts: Set[Fact[?]] = Set.empty,
  discarded: Set[Predicate] = Set.empty
) {

  import SelectedPredicatesAndSources.*

  def withPredicate(p: Predicate): SelectedPredicatesAndSources = {
    copy(
      predicates = predicates + (p -> p.facts),
      facts = facts ++ p.facts
    )
  }

  def withDiscard(p: Predicate): SelectedPredicatesAndSources = copy(discarded = discarded + p)
}

object SelectedPredicatesAndSources {
  lazy val empty: SelectedPredicatesAndSources = SelectedPredicatesAndSources()

  def apply[T: FactOps](start: Fact.Val[T]): SelectedPredicatesAndSources = {
    new SelectedPredicatesAndSources(
      sources = start.sources,
      facts = start.facts ++ start.predecessors
    )
  }
}
