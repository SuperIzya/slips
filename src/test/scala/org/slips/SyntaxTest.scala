package org.slips

import org.slips.SyntaxTest.SE
import org.slips.core.Fact
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Res
import org.slips.core.conditions.Predicate

object SyntaxTest {
  case class Data1(count: Int, name: String)
  case class Data2(fullName: String, points: Double)

  val mapFunction: (Data1, Data2) ⇒ Double        = _.count + _.points
  val predicateFunction: (Data2, Data1) ⇒ Boolean = _.points > _.count

  val SE = SimpleEnvironment
  import SE.*
  SE {
    val conditions1 = {
      import SE.Syntax.Conditions.*

      val testFacts: (Fact[Data1], Fact[Data2]) ⇒ Predicate = (f1, f2) ⇒
        (f2.test(_.fullName.isEmpty) && f1.value(_.name) === "abc") ||
          f2.test(_.points > 0)

      for {
        f1 ← all[Data1]
        f2 ← all[Data2] if f2.value(_.points) === 2
        _  ← (f2, f1).test { case (d1, d2) ⇒
          d1.points > d2.count
        }
        i = Fact.literal(2)
        _  ← Fact.literal(3) === f1.value(_.count)
        _  ← testFacts(f1, f2)
        f3 ← all[Data1] if f1 =!= f3
        g = (f1, f2).value(mapFunction.tupled)
        f = Fact.literal(3f)
        _ ← f3.value(_.count.toDouble) =!= g
      } yield (f1, f3, g, i, Fact.literal(2f))
      /*

      all[Data1].flatMap { f1 ⇒
        val x = all[Data2].withFilter(f2 ⇒ f2.value(_.points) === 2)
        x.flatMap { f2 ⇒
          val x: Predicate = (f2, f1).test { case (d1, d2) ⇒
            d1.points > d2.count
          }
          x.map { _ ⇒
              val i = Fact.literal(2); i
            }
            .flatMap(i ⇒
              (Fact.literal(3) === f1.value(_.count)).flatMap(_ ⇒
                testFacts(f1, f2).flatMap { _ ⇒
                  val x = all[Data1].withFilter(f3 ⇒ f1 =!= f3)
                  x.map { f3 ⇒
                      val g = (f1, f2).value(mapFunction.tupled);
                      (f3, g)
                    }
                    .map { case (f3, g) ⇒
                      val f = Fact.literal(3f); (f3, g, f)
                    }
                    .flatMap { case (f3, g, f) ⇒
                      (f3.value(_.count.toDouble) =!= g).map(_ ⇒ (f1, f3, g, i, Fact.literal(2f)))
                    }
                }
              )
            )
        }
      }
       */

    }
    val conditions2 = {
      import SE.Syntax.Conditions.{*, given}
      for {
        (f1, f2, _, _, _) ← conditions1
        f3                ← all[Data2] if f2.value(_.count) =!= 3
      } yield (f3, f1)
      /*conditions1
        .withFilter { case (f1, f2, _, _, _) ⇒ true }
        .flatMap { case (f1, f2, _, _, _) ⇒
          val x: Condition[Data2] = all[Data2].withFilter(f3 ⇒ f2.value(_.count) =!= 3)
          x.map(f3 ⇒ (f3, f1))
        }*/
    }

    val rule = {
      import SE.Syntax.Conditions.*
      import Syntax.Actions.*
      Rule("test")(conditions2) { case (f2, f1) ⇒
        for {
          d2 ← getValue(f2)
          _  ← assert(Data2("foo", d2.points))
        } yield ()
      }
    }
    rule
  }

}
