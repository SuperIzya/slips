package org.slips.core.build

import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.*

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
      facts = facts ++ p.facts,
      alphaSources = alphaSources ++ p.facts.flatMap(_.alphaSources),
      sources = sources ++ p.facts.flatMap(_.sources.map(_.signature))
    )
  }

  def withDiscard(p: Predicate): SelectedPredicatesAndSources = copy(discarded = discarded + p)
}

object SelectedPredicatesAndSources {
  lazy val empty: SelectedPredicatesAndSources = SelectedPredicatesAndSources()

  def apply[T: FactOps](start: Fact.Val[T]): SelectedPredicatesAndSources = {
    new SelectedPredicatesAndSources(
      sources = start.alphaSources.map(_.signature),
      facts = (start.facts ++ start.predecessors).toSet,
      alphaSources = start.alphaSources
    )
  }
}
