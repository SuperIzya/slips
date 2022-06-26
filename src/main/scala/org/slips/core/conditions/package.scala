package org.slips.core

import cats.data.State

package object conditions {
  type ParseStep[x] = State[Parser.Context, Fact.Val[x]]
}
