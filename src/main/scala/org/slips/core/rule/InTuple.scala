package org.slips.core.rule

import org.slips.NotTuple
import scala.util.NotGiven

trait InTuple[T, Q: NotTuple] {}

object InTuple {
  given equality[Q: NotTuple]: InTuple[Q, Q] with {}
  given lastElem[Q: NotTuple]: InTuple[Q *: EmptyTuple, Q] with    {}
  given headElem[T <: Tuple, Q: NotTuple]: InTuple[Q *: T, Q] with {}
  given tailStep[T <: Tuple, Q: NotTuple, P: NotTuple](
    using ev: NotGiven[Q =:= P],
    prev: InTuple[T, Q]
  ): InTuple[P *: T, Q] with {}
}
