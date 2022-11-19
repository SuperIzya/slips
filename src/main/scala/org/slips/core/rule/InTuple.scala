package org.slips.core.rule

import org.slips.NotTuple
import scala.util.NotGiven

trait InTuple[T, Q] {}

object InTuple {
  given equality[Q]: InTuple[Q, Q] with                  {}
  given lastElem[Q]: InTuple[Q *: EmptyTuple, Q] with    {}
  given headElem[T <: Tuple, Q]: InTuple[Q *: T, Q] with {}
  given tailStep[T <: Tuple, Q, P](
    using ev: NotGiven[Q =:= P],
    prev: InTuple[T, Q]
  ): InTuple[P *: T, Q] with {}
}
