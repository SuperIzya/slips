package org.slips.core.builder

import cats.data.State
import org.slips.core.conditions.Predicate
import org.slips.core.Fact

object BuildStep {
  def pure[T](v: Fact.Val[T]): BuildStep[T]          = State.pure(v)
  def modify(f: Context => Context): BuildStep[Unit] =
    State.modify(f).map(_ => Fact.unit)
}
