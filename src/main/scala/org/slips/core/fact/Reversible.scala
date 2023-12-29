package org.slips.core.fact

import Fact.*
import org.slips.core.predicates.Predicate
import scala.util.NotGiven

sealed trait Reversible[A, B] {
  def apply(a: A): Val[B]

  def flip(b: InverseVal[A]): B
}

object Reversible {
  given instance[A, B](using ev1: InverseVal[A] =:= B, ev2: Val[B] =:= A, ev3: NotGiven[A =:= Predicate]): Reversible[A, B]
  with {
    override def apply(a: A): Val[B] = ev2.flip(a)

    override def flip(b: InverseVal[A]): B = ev1(b)
  }
}
