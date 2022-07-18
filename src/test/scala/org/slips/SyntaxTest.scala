package org.slips

import org.slips.SyntaxTest.SE
import org.slips.core.Fact
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Res
import org.slips.core.predicates.Predicate

object SyntaxTest {
  case class Data1(count: Int, name: String)
  case class Data2(fullName: String, points: Double)
  case class Data3(price: Long, weight: Double)

  val mapFunction: (Data1, Data2) ⇒ Double        = _.count + _.points
  val predicateFunction: (Data2, Data1) ⇒ Boolean = _.points > _.count

  val SE                                                         = SimpleEnvironment
  val conditions1: Condition[(Data1, Data2, Double, Int, Float)] = SE {
    import SE.Syntax.Conditions.*

    val testFacts: (Fact[Data1], Fact[Data2]) ⇒ Predicate = (f1, f2) ⇒
      ((f2, f1).test { case (d2, d1) ⇒
        d2.points > d1.count
      } &&
        f2.test(_.fullName.isEmpty) && f1.value(_.name) === "abc") ||
        f2.test(_.points > 0)

    for {
      f1 ← all[Data1]
      f2 ← all[Data2] if f2.value(_.points) === 2
      _  ← (f2, f1).test { case (d2, d1) ⇒
        d2.points > d1.count
      }
      i = Fact.literal(2)
      _  ← Fact.literal(3) === f1.value(_.count)
      _  ← testFacts(f1, f2)
      f3 ← all[Data3] if f3.value(_.price) === 2
      g = (f1, f2).value(mapFunction.tupled)
      f = Fact.literal(3f)
      _ ← f3.value(_.weight) =!= 3.0 && f1.value(_.count) === i
    } yield (f1, f2, g, i, Fact.literal(2f))
  }

  val conditions2 = SE {
    import SE.Syntax.Conditions.*
    for {
      (f1, f2, _, _, _) ← conditions1
      f3                ← all[Data2] if f2.value(_.points) =!= 3.0
    } yield (f3, f1)
  }

  val rule = SE {
    import SE.Syntax.Conditions.*
    import SE.Syntax.Actions.*

    SE.Rule("test")(conditions2) { case (f2, f1) ⇒
      for {
        d2 ← getValue(f2)
        _  ← assert(Data2("foo", d2.points))
      } yield ()
    }
  }

}
