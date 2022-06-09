package org.slips.core

import cats.Id
import cats.data.{IndexedStateT, StateT}
import org.slips.{Environment, SimpleEnvironment}
import SimpleEnvironment.Syntax.*
object SyntaxTest {
  case class Data1(count: Int, name: String)
  case class Data2(fullName: String, points: Double)

  val SE = SimpleEnvironment
  val conditions = SimpleEnvironment {
    for {
      f1 <- all[Data1]
      f2 <- all[Data2] if f2.get(_.points) === 2
      _  <- f1.get(_.count) === 3
      _  <- f2.map(_.fullName.isEmpty) && f1.get(_.name) === "abc" || f2.map(_.points > 0)
      f3 <- all[Data1]
      _  <- f3.get(_.count) =!= 5 && f1 =!= f3
    } yield f1 *: f3 *: EmptyTuple
  }

  val conditions2 = SimpleEnvironment {
    for {
      f <- conditions
      (f1, f2) = f
      f3 <- all[Data2] if f2.get(_.count) =!= 3
    } yield f3 *: f1 *: EmptyTuple
  }

  val rule =
    SimpleEnvironment {

      val rule = conditions2.makeRule("test") {
        case (f1, f2) =>
          for {
            d1 <- getValue(f1)
            _  <- assert(Data2("foo", d1.points))
          } yield ()
      }

      rule
    }
}
