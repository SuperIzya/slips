package org.slips.syntax

import scala.util.NotGiven

trait Expression[+T] {
  def map[Q](f: T => Q): Expression[Q]                 = ???
  def flatMap[Q](f: T => Expression[Q]): Expression[Q] = ???
  def toTuple(using NotGiven[T <:< Tuple]): Expression[T *: EmptyTuple]
}
