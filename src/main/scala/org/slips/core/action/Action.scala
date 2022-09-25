package org.slips.core.action

import cats.data.StateT
import org.slips.Environment

object Action {
  def apply[Q](
    using env: Environment
  )(
    f: env.Action.Method[Q]
  ): env.Action[Q] = StateT(f)

  def pure[Q](q: => Q)(using env: Environment): env.Action[Q] = StateT.pure[env.Effect, env.Action.Context, Q](q)
}
