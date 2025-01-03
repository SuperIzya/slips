package org.slips.core.network

import org.slips.core.WithSignature
import org.slips.core.build.BuildStep
import org.slips.core.build.EnvBuildStep

private[slips] trait Node[F[_]] extends WithSignature {
  def materialize: BuildStep[F][materialized.Node[F]] = ???
}
