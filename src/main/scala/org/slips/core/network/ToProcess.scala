package org.slips.core.network

import org.slips.core.fact.Fact
import org.slips.core.network.Chain

case class ToProcess(fact: Fact.Source[?], chains: Set[Chain])

object ToProcess {
  given Ordering[ToProcess] = Ordering.fromLessThan[ToProcess]((x, y) => x.chains.size < y.chains.size)
}
