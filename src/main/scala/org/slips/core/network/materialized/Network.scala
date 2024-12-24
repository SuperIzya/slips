package org.slips.core.network.materialized

import org.slips.core.network.AlphaNode

case class Network[F[_]](nodes: Map[AlphaNode[F], Node.Alpha[F]])
