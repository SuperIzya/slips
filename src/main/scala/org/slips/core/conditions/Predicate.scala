package org.slips.core.conditions

import org.slips.Signature
import org.slips.core.*
import org.slips.core.fact.*

import scala.annotation.targetName

sealed trait Predicate extends WithSignature { self =>
  def signature: Signature
  def facts: Set[Fact.Source[?]]
}

object Predicate {
  final case class Test[T](signature: Signature, test: T => Boolean, rep: Fact.Val[T])(using val T: FactOps[T], val sourceLocation: SourceLocation)
      extends Predicate { self =>
    val facts: Set[Fact.Source[?]] = T.sources(rep)
    def not: Predicate             = copy(
      signature = Signature.DerivedUnary(signature, "!" + _),
      test = (t: T) => !self.test(t)
    )
  }

  @targetName("and")
  final case class &&(left: Predicate, right: Predicate) extends Predicate {
    val facts: Set[Fact.Source[?]] = left.facts ++ right.facts

    val signature: Signature = Signature.DerivedBinary(left.signature, right.signature, (l, r) => s"$l && $r")
  }

  @targetName("or")
  final case class ||(left: Predicate, right: Predicate) extends Predicate {
    val facts: Set[Fact.Source[?]] = left.facts ++ right.facts
    val signature: Signature       = Signature.DerivedBinary(left.signature, right.signature, (l, r) => s"$l || $r")
  }

  @targetName("not")
  final case class ! private(p: Predicate) extends Predicate {
    val facts: Set[Fact.Source[?]] = p.facts
    val signature: Signature       = Signature.DerivedUnary(p.signature, p => s"!$p")
  }

  @targetName("not")
  object ! {
    def apply(p: Predicate)(using DummyImplicit): Predicate =
      p match {
        case !(pp)        => pp
        case e: Exist[?]    => e.not
        case n: NotExist[?] => n.not
        case _              => new !(p)
      }
  }

  extension (self: Predicate) {
    @targetName("and")
    def &&(right: Predicate): Predicate = Predicate.&&(self, right)
    @targetName("or")
    def ||(right: Predicate): Predicate = Predicate.||(self, right)
    @targetName("not")
    def unary_! : Predicate = Predicate.!(self)
  }


  final case class NotExist[T : {FactOps, ScalarFact}](f: Fact[T])(using val sourceLocation: SourceLocation)
      extends Predicate {
    val facts: Set[Fact.Source[?]] = Set(f.source)
    val signature: Signature       = f.signature.andThen(s => s"NotExist[$s]")

    private[slips] def not: Predicate = Exist(f)
  }

  final case class Exist[T : {FactOps, ScalarFact}](f: Fact[T])(using val sourceLocation: SourceLocation)
      extends Predicate {
    val facts: Set[Fact.Source[?]] = Set(f.source)
    val signature: Signature       = f.signature.andThen(s => s"Exist[$s]")

    private[slips] def not: Predicate = NotExist(f)
  }

  given CanEqual[Predicate, Predicate] = CanEqual.derived

}
