package org.slips.core.action

trait Context[F[_]](facts: Map[FactId[_], Any]) {
  def getValue[Q](fact: FactId[Q]): F[(Context[F], Q)]

  def assert[Q](q: Q): F[(Context[F], Unit)]

  def remove[Q](fact: FactId[Q]): F[(Context[F], Unit)]
}
