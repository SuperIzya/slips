package org.slips.core.network.materialized

trait Publisher[T] {
  def map[Q](
    f: T => Q
  ): Publisher[Q]
  def flatMap[Q](
    f: T => Publisher[Q]
  ): Publisher[Q]
}
