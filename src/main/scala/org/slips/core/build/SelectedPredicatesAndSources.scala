package org.slips.core.build

import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.*

private[slips] case class SelectedPredicatesAndSources(
  predicates: Set[Predicate] = Set.empty,
  signatures: Set[Signature] = Set.empty,
  facts: Set[Fact.Source[?]] = Set.empty,
  discarded: Set[Predicate] = Set.empty
) {

  import SelectedPredicatesAndSources.*

  def sourceSignatures: Set[Signature] = facts
    .collect {
      case _: Fact.Literal[_] => Set.empty
      case s: Fact.Source[_]  => Set(s.signature)
    }
    .flatten

  def withPredicate(p: Predicate): SelectedPredicatesAndSources = {
    copy(
      predicates = predicates + p,
      facts = facts ++ p.facts,
      signatures = signatures + p.signature
    )
  }

  def withDiscard(p: Predicate): SelectedPredicatesAndSources = copy(discarded = discarded + p)
}

private[slips] object SelectedPredicatesAndSources {
  lazy val empty: SelectedPredicatesAndSources = SelectedPredicatesAndSources()

  def apply[T: { FactOps as T }](start: Fact.Val[T]): SelectedPredicatesAndSources =
    new SelectedPredicatesAndSources(facts = T.sources(start))
}
