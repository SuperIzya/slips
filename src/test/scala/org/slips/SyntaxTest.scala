package org.slips

import org.slips.SyntaxTest.SE
import org.slips.core.conditions.{Condition, Predicate}
import org.slips.core.conditions.Condition.Res
import org.slips.core.data.Fact

object SyntaxTest {
  case class Data1(count: Int, name: String)
  case class Data2(fullName: String, points: Double)

  val mapFunction: (Data1, Data2) => Double = _.count + _.points
  val predicateFunction: (Data2, Data1) => Boolean = _.points > _.count

  val SE = SimpleEnvironment
  import SE.*
  SE {
    val conditions1 = {
      import SE.Syntax.Conditions.*
      for {
        f1 <- all[Data1]
        f2 <- all[Data2] if f2.value(_.points) === 2
        _ <- (f2, f1).test {
          case (d1, d2) => d1.points > d2.count
        }
        i = Fact.literal(2)
        _ <- Fact.literal(3) === f1.value(_.count)
        _ <- (f2.test(_.fullName.isEmpty) && f1.value(_.name) === "abc") ||
          f2.test(_.points > 0)
        f3 <- all[Data1] if f1 =!= f3
        g = (f1, f2).value(mapFunction.tupled)
        f = Fact.literal(3f)
        _ <- f3.value(_.count) =!= g
      } yield (f1, f3, g, i, Fact.literal(2f))
    }

    val conditions2 = {
      import SE.Syntax.Conditions.*
      for {
        (f1, f2, _, _, _) <- conditions1
        f3 <- all[Data2] if f2.value(_.count) =!= 3
      } yield (f3, f1)
    }

    val rule = {
      import SE.Syntax.Conditions.*
      import Syntax.Actions.*
      Rule("test")(conditions2) {
        case (f2, f1) =>
          for {
            d2 <- getValue(f2)
            _ <- assert(Data2("foo", d2.points))
          } yield ()
      }
    }
    rule
  }

}
