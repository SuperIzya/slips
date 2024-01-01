package org.slips.core.conditions

import cats.Monoid
import org.slips.Environment
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core.*
import org.slips.core.build.BuildStep
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.ParseStep
import org.slips.core.fact.*
import org.slips.core.fact.Fact.TMap
import org.slips.core.network.alpha.AlphaNode
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Predicate extends WithSignature { self =>
  // TODO: Have here TASTy of the method that tests the facts ???
  override def signature: Signature = Signature.Manual(self.getClass.getSimpleName)
  def facts: Set[Fact[?]]

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

  object IsAlpha {
    def unapply(p: Predicate): Option[(Predicate, Fact.Alpha[?])] = Option.when {
      val sources = p.facts.flatMap(_.alphaSources)
      sources.size == 1 && p.facts.forall(_.isAlpha)
    } {
      p -> p.facts.head.alphaSources.head
    }
  }

  object IsBeta {
    def unapply(p: Predicate): Option[Predicate] = Option
      .when(p.facts.exists(!_.isAlpha) || p.facts.flatMap(_.alphaSources).size > 1)(p)
  }

  def add(p: Predicate): ParseStep[Unit] = p match {
    case And(left, right) =>
      for {
        _ <- add(left)
        _ <- add(right)
      } yield Fact.unit

    case _ => ParseStep.modify(_.addPredicate(p))
  }

  final case class Test[T: FactOps](
    override val signature: Signature,
    test: T => Boolean,
    rep: Fact.Val[T]
  ) extends Predicate { self =>
    override val facts: Set[Fact[?]] = rep.facts.toSet
    def not: Predicate               = copy(
      signature = Signature.DerivedUnary(signature, "!" + _),
      test = (t: T) => !self.test(t)
    )
  }

  final case class And(
    left: Predicate,
    right: Predicate
  ) extends Predicate {
    override lazy val facts: Set[Fact[?]] = left.facts ++ right.facts

    override val signature: Signature = Signature.DerivedBinary(left.signature, right.signature, (l, r) => s"$l && $r")
  }

  final case class Or(
    left: Predicate,
    right: Predicate
  ) extends Predicate {
    override lazy val facts: Set[Fact[?]] = left.facts ++ right.facts
    override val signature: Signature = Signature.DerivedBinary(left.signature, right.signature, (l, r) => s"$l || $r")
  }

  case class Not private (p: Predicate) extends Predicate {
    override val facts: Set[Fact[?]]  = p.facts
    override val signature: Signature = Signature.DerivedUnary(p.signature, p => s"!$p")
  }

  object Not {
    def apply(p: Predicate)(using DummyImplicit): Predicate =
      p match
        case Not(pp)        => pp
        case e: Exist[?]    => e.not
        case n: NotExist[?] => n.not
        case _              => new Not(p)
  }

  final case class NotExist[T : FactOps : ScalarFact](f: Fact[T]) extends Predicate {
    override lazy val facts: Set[Fact[?]] = f.predecessors.toSet + f
    override val signature: Signature     = f.signature.andThen(s => s"NotExist[$s]")

    private[slips] def not: Predicate = Exist(f)
  }

  final case class Exist[T : FactOps : ScalarFact](f: Fact[T]) extends Predicate {
    override lazy val facts: Set[Fact[?]] = f.predecessors.toSet + f
    override val signature: Signature     = f.signature.andThen(s => s"Exist[$s]")

    private[slips] def not: Predicate = NotExist(f)
  }
}
