package org.slips.core.network.materialized

import org.slips.core.network.BetaNode

case class BetaNetwork(
  alphaNetwork: AlphaNetwork,
  nodes: Map[BetaNode, Node.Beta]
)
