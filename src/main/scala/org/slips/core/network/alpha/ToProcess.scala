package org.slips.core.network.alpha

import org.slips.core.fact.Fact
import org.slips.core.network.alpha.Chain

case class ToProcess(fact: Fact.Alpha[?], chains: Set[Chain])

object ToProcess {
  given Ordering[ToProcess] = Ordering.fromLessThan[ToProcess]((x, y) => x.chains.size < y.chains.size)
}
