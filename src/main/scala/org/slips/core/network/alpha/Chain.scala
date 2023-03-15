package org.slips.core.network.alpha

import org.slips.core.network.AlphaNode
import org.slips.core.network.AlphaNode.Sources

private[network] case class Chain(head: AlphaNode, tail: Option[Chain], facts: Sources)

private[network] object Chain {

  given Ordering[Chain] = (x, y) => x.facts.sizeCompare(y.facts)

  def apply(predicate: AlphaNode => AlphaNode, tail: Chain, facts: Sources): Chain =
    Chain(predicate(tail.head), Some(tail), facts)
}
