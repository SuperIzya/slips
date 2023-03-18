package org.slips.core.network

import org.slips.core.build.BuildStep

private[slips] trait Node {
  def signature: String
  def materialize: BuildStep[materialized.Node] = ???
}
