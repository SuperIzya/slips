package org.slips.core

import org.scalatest.funsuite.AnyFunSuiteLike
import org.slips.core.conditions.Condition
import org.slips.core.fact.{Fact, FactOps}
import org.slips.syntax.*

import scala.annotation.tailrec
import scala.util.NotGiven

class ConditionTest extends AnyFunSuiteLike {

  case class Data1(
    i: Int,
    s: String
  )

  case class Data2(
    s: String,
    i: Int
  )

  case class Data3(
    d: Double,
    f: Float,
    b: Boolean
  )

  test("flatMap is stack-safe") {
    inline def flatMap[T](
      count: Int
    )(using
      NotGiven[T <:< Tuple],
      Fact[T] =:= Fact.Val[T],
      FactOps[T]
    ): Condition[T] = {
      @tailrec
      def work(
        left: Int,
        current: Condition[T]
      ): Condition[T] = {
        if (left == 0) current
        else work(left - 1, current.flatMap(_ => Condition.all[T]))
      }
      work(count, Condition.all[T])
    }

    val f1 = flatMap[Data1](165536)
    val f2 = flatMap[Data2](165536)
    val f3 = flatMap[Int](165536)

    succeed
  }
}
