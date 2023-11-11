package org.slips.core

import cats.data.State
import org.slips.core.conditions.Predicate
import org.slips.core.fact.Fact

package object conditions {
  type ParseStep = [x] =>> State[Parser.Context, Fact.Val[x]]
  type Source    = [x] =>> Condition.Source[x]
  type Test      = [x] =>> Predicate.Test[x]
}
