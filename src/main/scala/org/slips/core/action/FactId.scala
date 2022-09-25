package org.slips.core.action

import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.action
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps

sealed trait FactId[T: NotTuple] {
  def signature: String
  def facts: Fact.Val[T]

  def value(using env: Environment): env.Action[T] = action.Action(_.getValue(this))
}

object FactId {
  type Val[T] = T match
    case Tuple => Tuple.Map[T, FactId]
    case _     => FactId[T]
}
