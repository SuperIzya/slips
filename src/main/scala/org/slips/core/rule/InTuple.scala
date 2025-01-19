package org.slips.core.rule

import org.slips.NotTuple
import scala.util.NotGiven

trait InTuple[T, Q] {}

object InTuple {
  given [Q] => InTuple[Q, Q]                            {}
  given lastElem: [Q] => InTuple[Q *: EmptyTuple, Q]    {}
  given headElem: [T <: Tuple, Q] => InTuple[Q *: T, Q] {}

  given tailStep: [T <: Tuple, Q, P] => NotGiven[Q =:= P] => InTuple[T, Q] => InTuple[P *: T, Q] {}
}
