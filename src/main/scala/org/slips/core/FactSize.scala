package org.slips.core

import scala.util.NotGiven

sealed trait FactSize[T] {
  val size: Int
}

object FactSize {

  given empty: FactSize[EmptyTuple] with {
    override val size: Int = 0
  }

  given tuple[H, T <: Tuple](using T: FactSize[T]): FactSize[H *: T] with {
    override val size: Int = 1 + T.size
  }

  given scalar[T](using NotGiven[T <:< Tuple]): FactSize[T] with {
    override val size: Int = 1
  }
}
