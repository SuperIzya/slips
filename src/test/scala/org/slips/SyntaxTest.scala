package org.slips

import org.slips.syntax.*
import org.slips.core.Quantor.*
import org.slips.Fact.*
import org.slips.syntax.Test.*

class SyntaxTest {

  case class Data1(str: String, i: Int)
  case class Data2(f: Float, d: Double)
  case class Data3(d1: Data1, d2: Data2)

  val C: Facts[(Data1, Data2, Data3)] = Facts(All[Data1] *: All[Data2] *: All[Data3] *: EmptyTuple)
  val res: Condition[(Data1, Data3)] = C
    .condition {
      case (f1, f2, f3) =>
        test(f1)(_.str != "") and
          (test(f2, f3)((x2, x3) => x2.f != x3.d2.f) or
            test((f1, f2, f3)) {
              case (x1, x2, x3) => x1.str != "" && x2.f != x3.d2.f
            })
    }
    .map {
      case (x1, x2, x3) => (x1, x3)
    }

  val res2: Condition[(Data1, Data3)] = Condition {
    for {
      x1 <- All[Data1]
      _  <- x1.str != ""
      x2 <- All[Data2]
      x3 <- All[Data3]
      _  <- x2.f != x3.d2.f || (x1.str != "" && x2.f != x3.d2.f)
    } yield (x1, x3)
    /*
    All[Data1]
      .flatMap(x1 =>
        (x1.str != "")
          .flatMap(_ =>
            All[Data2]
              .flatMap(x2 =>
                All[Data3]
                  .flatMap(x3 =>
                    (x2.f != x3.d2.f || (x1.str != "" && x2.f != x3.d2.f))
                      .map(_ => (x1, x3))
                  )
              )
          )
      )
   */
  }

  val res3: Condition[Data1] = Condition { All[Data1] }
}
