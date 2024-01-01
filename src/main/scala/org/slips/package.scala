package org

import org.slips.core.rule.Rule.RuleAction
import scala.util.NotGiven

package object slips {
  type Env[T]      = Environment ?=> T
  type EnvRule[T]  = (e: Environment) ?=> e.Rule[T]
  type NotTuple[T] = NotGiven[T <:< Tuple]
}
