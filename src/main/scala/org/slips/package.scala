package org

import org.slips.core.rule.Rule

import scala.util.NotGiven

package object slips {
  type Env[T] = Environment ?=> T
  type EnvF[F[_[_]]] = (env: Environment) ?=> F[env.Effect]
  type EnvRule = EnvF[Rule]
  type NotTuple[T] = NotGiven[T <:< Tuple]
}
