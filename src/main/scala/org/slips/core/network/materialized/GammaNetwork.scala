package org.slips.core.network.materialized

import org.slips.core.network.GammaNode

case class GammaNetwork(
  betaNetwork: BetaNetwork,
  nodes: Map[GammaNode, Node.Gamma]
)
