package org.slips.core.network.materialized

trait Publisher[F[_], T] {
  def map[Q](f: T => Q): Publisher[F, Q]
  def flatMap[Q](f: T => Publisher[F, Q]): Publisher[F, Q]
}
