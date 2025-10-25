package org.slips.generator

import org.slips.core.conditions.Predicate
import org.slips.core.fact.Fact
import org.slips.data.*
import org.slips.data.Data.*
import org.slips.generator.*
import org.slips.syntax.*
import scala.annotation.tailrec
import zio.test.*

object Condition {
  import Predicate.*

  sealed trait PredicateOps {
    def combine(p1: Predicate, p2: Predicate): Predicate
  }
  object PredicateOps       {
    case object And extends PredicateOps {
      override def combine(p1: Predicate, p2: Predicate): Predicate = p1 && p2
    }

    case object Or extends PredicateOps {
      override def combine(p1: Predicate, p2: Predicate): Predicate = p1 || p2
    }

    case object Not extends PredicateOps {
      override def combine(p1: Predicate, p2: Predicate): Predicate = (p1, p2) match {
        case (_ || _, _) => !p1 || p2
        case (_, a && b) => p1 && !a && !b
        case (a && b, _) => a && !b && !p2
        case (_, _)      => p1 || !p2
      }
    }
    def next: DGen[PredicateOps] = Gen.elements(And, Or, Not)
  }

  def nextAlphaPredicate[D <: Data](data: D)(using pa: GenAlpha[D], D: DGen[D]): AlphaGen[D] = D.flatMap(pa.next)

  def nextAlphaPredicates[D <: Data](data: D, count: Int)(using pa: GenAlpha[D], D: DGen[D]): AlphaGen[D] = {
    val genNext = D.flatMap(pa.next)

    @tailrec
    def run(g: AlphaGen[D], rest: Int): AlphaGen[D] = {
      if rest > 0 then {
        val next: AlphaGen[D] = for
          prev <- g
          n    <- genNext
          ops  <- PredicateOps.next
        yield f => ops.combine(prev(f), n(f))
        run(next, rest - 1)
      } else g
    }
    run(genNext, count)
  }

}
