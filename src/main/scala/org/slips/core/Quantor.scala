package org.slips.core

import org.slips.syntax.Expression
import scala.util.NotGiven

sealed trait Quantor[+T] extends Expression[T] {
  def flatMap[Q](t: T => Quantor[Q]): Quantor[Q] = ???
}

object Quantor {
  case class All[T] private() extends Quantor[T] {
    override def toTuple(using NotGiven[T <:< Tuple]): All[T *: EmptyTuple] = All[T *: EmptyTuple]
  }
  object All {
    def apply[T](using DummyImplicit): All[T] = All[T]()
  }
}
