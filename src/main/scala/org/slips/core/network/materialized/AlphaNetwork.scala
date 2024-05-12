package org.slips.core.network.materialized

import org.slips.core.network.AlphaNode

case class AlphaNetwork(
  nodes: Map[AlphaNode, Node.Alpha]
)
