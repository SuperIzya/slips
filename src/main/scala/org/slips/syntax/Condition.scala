package org.slips.syntax

import org.slips.core.Quantor
import org.slips.macros.ConditionBuilder
import scala.util.NotGiven

trait Condition[T] {
  def map[Q](f: T => Q): Condition[Q] = ???
}

object Condition {
  inline def apply[T](inline expr: => Expression[T]): Condition[T] = ConditionBuilder(expr)
}
