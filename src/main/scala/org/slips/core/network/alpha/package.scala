package org.slips.core.network

import cats.data.State

package object alpha {
  private[network] type FoldState[T] = State[FactsFolder, T]
}
