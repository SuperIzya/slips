package org.slips.core.network

import org.slips.core.WithSignature
import org.slips.core.build.BuildStep

private[slips] trait Node extends WithSignature {
  def materialize: BuildStep[materialized.Node] = ???
}
