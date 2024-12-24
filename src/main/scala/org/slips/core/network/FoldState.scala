package org.slips.core.network

import cats.data.State

private[network] object FoldState {
  def apply[T](f: FactsFolder => (FactsFolder, T)): FoldState[T] = State(f)

  def modify(f: FactsFolder => FactsFolder): FoldState[Unit] = State.modify(f)

  def get: FoldState[FactsFolder] = State.get

  def pure[T](t: => T): FoldState[T] = State.pure(t)
}
