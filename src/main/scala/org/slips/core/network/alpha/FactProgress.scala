package org.slips.core.network.alpha

import org.slips.core.fact.Fact
import org.slips.core.network.AlphaNode

trait FactProgress  {
  val fact: Fact.Source
  val chains: Set[Chain]
  val topNode: AlphaNode
}

object FactProgress {

  case class InProgress(
    fact: Fact.Source,
    chains: Set[Chain],
    united: Set[Chain],
    left: Set[Chain],
    topNode: AlphaNode.Combine
  ) extends FactProgress

  case class Done(
    fact: Fact.Source,
    chains: Set[Chain],
    topNode: AlphaNode
  ) extends FactProgress

}
