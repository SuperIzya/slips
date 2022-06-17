package org.slips.core

import cats.data.State
import org.slips.core.Fact

package object builder {
  type BuildStep[x] = State[Context, Fact.Val[x]]
}
