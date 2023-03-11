package org.slips.core.conditions

import cats.data.State
import org.slips.core.fact.Fact

object ParseStep {
  def pure[T](
    v: Fact.Val[T]
  ): ParseStep[T] = State.pure(v)
  def modify(
    f: Parser.Context => Parser.Context
  ): ParseStep[Unit] =
    State.modify(f).map(_ => Fact.unit)
}
