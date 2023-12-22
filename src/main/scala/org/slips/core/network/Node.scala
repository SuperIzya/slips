package org.slips.core.network

import org.slips.core.Signed
import org.slips.core.build.BuildStep

private[slips] trait Node extends Signed {
  def materialize: BuildStep[materialized.Node]
}
