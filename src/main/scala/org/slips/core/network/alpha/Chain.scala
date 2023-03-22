package org.slips.core.network.alpha

import org.slips.core.build.AlphaPredicate
import org.slips.core.fact.Fact

private[network] case class Chain private (
  head: AlphaPredicate,
  tail: Option[Chain],
  facts: Set[Fact.Alpha[_]],
  predicates: Set[String]
)

private[network] object Chain {

  given Ordering[Chain] = (x, y) => x.facts.sizeCompare(y.facts)

  def apply(predicate: AlphaPredicate, tail: Chain): Chain =
    Chain(predicate, Some(tail), predicate.facts, tail.predicates + predicate.predicate.signature)
}
