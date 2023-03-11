package org

import org.slips.core.rule.Rule.RuleAction
import scala.util.NotGiven

package object slips {
  type Env[T]       = Environment ?=> T
  type NotTuple[T]  = NotGiven[T <:< Tuple]
  type EnvAction[T] = (
    env: Environment
  ) ?=> RuleAction[env.Effect, T]
}
