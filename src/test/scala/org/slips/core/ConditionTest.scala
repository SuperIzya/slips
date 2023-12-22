package org.slips.core

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuiteLike
import org.slips.NotTuple
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.fact.ScalarFact
import org.slips.syntax.*
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import scala.annotation.tailrec
import scala.util.NotGiven

class ConditionTest extends AnyFunSuiteLike {

  case class Data1(i: Int, s: String)

  case class Data2(s: String, i: Int)

  case class Data3(d: Double, f: Float, b: Boolean)

  test("flatMap is stack-safe") {
    inline def flatMap[T : ScalarFact : NotTuple : FactOps](count: Int): Condition[T] = {
      @tailrec
      def work(left: Int, current: Condition[T]): Condition[T] = {
        if (left == 0) current
        else work(left - 1, current.flatMap(_ => Condition.all[T]))
      }
      work(count, Condition.all[T])
    }

    val f1 = flatMap[Data1](65536)
    val f2 = flatMap[Data2](65536)
    val f3 = flatMap[Int](65536)

    succeed
  }
}
