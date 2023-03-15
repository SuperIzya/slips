package org.slips.core.network.alpha

import org.slips.core.fact.Fact
import org.slips.core.network.alpha.Chain

case class ToProcess(fact: Fact.Source, chains: Set[Chain])

object ToProcess {
  given Ordering[ToProcess] = (x, y) => -x.chains.sizeCompare(y.chains)
}
