package org.slips.syntax

import org.slips.Fact
import org.slips.core.Quantor

import Tuple.*
import scala.util.NotGiven

trait Facts[T <: Tuple] {
  val quantors: Tuple.Map[T, Quantor]

  def condition(usage: Tuple.Map[T, Fact] => Test)(using T <:< Tuple): Condition[T] = ???
  def map[Q](f: T => Q): Condition[Q]                            = ???
}
object Facts {

  def single[T](q: Quantor[T]): Facts[Tuple1[T]] = new Facts[Tuple1[T]]{
    override val quantors: Quantor[T] *: EmptyTuple = Tuple(q)
  }

  def apply[T <: Tuple](q: Tuple.Map[T, Quantor]): Facts[T] = new Facts[T] {
    override val quantors: Tuple.Map[T, Quantor] = q
  }

}
