package org.slips.core

import cats.data.State
import org.slips.core.fact.Fact

package object conditions {
  type ParseStep = [x] =>> State[Parser.Context, Fact.Val[x]]

}
