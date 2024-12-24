package org.slips.core.conditions

import org.slips.Signature
import org.slips.core.*
import org.slips.core.fact.*
import scala.annotation.tailrec
import scala.annotation.targetName

sealed trait Predicate extends WithSignature { self =>
  def signature: Signature
  def facts: Set[Fact.Source[?]]

  def and(other: Predicate): Predicate = Predicate.And(self, other)

  def or(other: Predicate): Predicate = Predicate.Or(self, other)

  @targetName("and_op")
  def &&(other: Predicate): Predicate = and(other)

  @targetName("or_op")
  def ||(other: Predicate): Predicate = or(other)

  @targetName("not_op")
  def unary_! : Predicate = Predicate.Not(self)

  def toDNF: Predicate = {
    import Predicate.*
    self match {
      case And(left, right) =>
        (left.toDNF, right.toDNF) match {
          case (Or(l1, r1), Or(l2, r2)) => ((l1 && l2) || (l1 && r2) || (r1 && l2) || (r1 && r2)).toDNF
          case (Or(l, r), right)        => ((l && right) || (r && right)).toDNF
          case (left, Or(l, r))         => ((left && l) || (left && r)).toDNF
          case (l, r)                   => l && r
        }
      case Not(p)           =>
        p.toDNF match {
          case And(left, right) => Not(left).toDNF || Not(right).toDNF
          case Or(left, right)  => (Not(left) && Not(right)).toDNF
          case Not(p1)          => p1
          case e: Exist[?]      => e.not
          case n: NotExist[?]   => n.not
          case t: Test[?]       => t.not
        }
      case Or(left, right)  => left.toDNF || right.toDNF
      case _                => self
    }
  }
}

object Predicate {
  final case class Test[T](
    signature: Signature,
    test: T => Boolean,
    rep: Fact.Val[T]
  )(using val T: FactOps[T])
      extends Predicate { self =>
    val facts: Set[Fact.Source[?]] = rep.sources
    def not: Predicate             = copy(
      signature = Signature.DerivedUnary(signature, "!" + _),
      test = (t: T) => !self.test(t)
    )
  }

  final case class And(
    left: Predicate,
    right: Predicate
  ) extends Predicate {
    val facts: Set[Fact.Source[?]] = left.facts ++ right.facts

    val signature: Signature = Signature.DerivedBinary(left.signature, right.signature, (l, r) => s"$l && $r")
  }

  final case class Or(
    left: Predicate,
    right: Predicate
  ) extends Predicate {
    val facts: Set[Fact.Source[?]] = left.facts ++ right.facts
    val signature: Signature       = Signature.DerivedBinary(left.signature, right.signature, (l, r) => s"$l || $r")
  }

  final case class Not private (p: Predicate) extends Predicate {
    val facts: Set[Fact.Source[?]] = p.facts
    val signature: Signature       = Signature.DerivedUnary(p.signature, p => s"!$p")
  }

  object Not {
    def apply(p: Predicate)(using DummyImplicit): Predicate =
      p match {
        case Not(pp)        => pp
        case e: Exist[?]    => e.not
        case n: NotExist[?] => n.not
        case _              => new Not(p)
      }
  }

  final case class NotExist[T : FactOps : ScalarFact](f: Fact[T]) extends Predicate {
    val facts: Set[Fact.Source[?]] = Set(f.source)
    val signature: Signature       = f.signature.andThen(s => s"NotExist[$s]")

    private[slips] def not: Predicate = Exist(f)
  }

  final case class Exist[T : FactOps : ScalarFact](f: Fact[T]) extends Predicate {
    val facts: Set[Fact.Source[?]] = Set(f.source)
    val signature: Signature       = f.signature.andThen(s => s"Exist[$s]")

    private[slips] def not: Predicate = NotExist(f)
  }

  given CanEqual[Predicate, Predicate] = CanEqual.derived
}
