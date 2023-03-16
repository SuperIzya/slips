package org.slips.core.build

import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.predicates.Predicate

case class SelectedPredicatesAndSources(
  alphaFacts: AlphaFacts = Map.empty,
  betaFacts: BetaFacts = Map.empty,
  sources: Set[Condition.Source[_]] = Set.empty,
  alphaSources: Set[Fact.Source] = Set.empty,
  facts: Set[Fact[_]] = Set.empty,
  discarded: Set[Predicate] = Set.empty
) {

  import SelectedPredicatesAndSources._

  def withPredicate(p: Predicate): SelectedPredicatesAndSources = p match {
    case pa: Predicate.Alpha =>
      copy(
        alphaFacts = addToMap(pa, alphaFacts),
        facts = facts ++ p.sourceFacts
      )
    case pb: Predicate.Beta  =>
      copy(
        betaFacts = addToMap(pb, betaFacts),
        facts = facts ++ p.sourceFacts
      )
  }

  def withDiscard(p: Predicate): SelectedPredicatesAndSources = copy(discarded = discarded + p)
}

object SelectedPredicatesAndSources {
  lazy val empty: SelectedPredicatesAndSources = SelectedPredicatesAndSources()

  private inline def addToMap[T <: Predicate, K](p: T, map: Map[K, T]): Map[K, T] = {
    map ++ p.facts.flatMap(f => f.sourceFacts).map { f => f -> (map.getOrElse(f, Set.empty) + p) }
  }

  def apply[T](start: Fact.Val[T])(using T: FactOps[T]): SelectedPredicatesAndSources = {
    new SelectedPredicatesAndSources(
      sources = T.sources(start),
      facts = T.sourceFacts(start)
    )
  }
}
