package org.slips.core

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.slips.{Environment, SimpleEnvironment}
import org.slips.core.conditions.Condition
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import scala.annotation.tailrec

class ConditionTest extends AnyFunSuiteLike {
  case class Data1(i: Int, s: String)
  case class Data2(s: String, i: Int)
  case class Data3(d: Double, f: Float, b: Boolean)

  given Environment = SimpleEnvironment

  test("flatMap is stack-safe") {
    def flatMap[T](count: Int): Condition[T] = {
      @tailrec
      def work(left: Int, current: Condition[T]): Condition[T] = {
        if (left == 0) current
        else work(left - 1, current.flatMap(_ => Condition.All[T]()))
      }
      work(count, Condition.All[T]())
    }

    val f1 = flatMap[Data1](65536)
    val f2 = flatMap[Data2](65536)
    val f3 = flatMap[Int](65536)

    succeed
  }
}
