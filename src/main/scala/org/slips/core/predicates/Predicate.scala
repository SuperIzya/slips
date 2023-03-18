package org.slips.core.predicates

import cats.Monoid
import org.slips.NotTuple
import org.slips.core.*
import org.slips.core.build.BuildStep
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.ParseStep
import org.slips.core.fact.*
import org.slips.core.fact.Fact.TMap
import org.slips.core.fact.FactOps.TupleOps
import org.slips.core.network.AlphaNode
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Predicate extends Signed {
  override val signature: String = this.getClass.getSimpleName
  def facts: Set[Fact[_]]

  def and(other: Predicate): Predicate = Predicate.And(this, other)

  def or(other: Predicate): Predicate = Predicate.Or(this, other)

  @targetName("and_op")
  def &&(other: Predicate): Predicate = and(other)

  @targetName("or_op")
  def ||(other: Predicate): Predicate = or(other)

  @targetName("not_op")
  def unary_! : Predicate = Predicate.Not(this)

  def toDNF: Predicate = {
    import Predicate.*
    this match {
      case AlphaTest(_, _, _) => this
      case And(left, right)   =>
        (left.toDNF, right.toDNF) match {
          case (Or(l1, r1), Or(l2, r2)) => ((l1 && l2) || (l1 && r2) || (r1 && l2) || (r1 && r2)).toDNF
          case (Or(l, r), right)        => ((l && right) || (r && right)).toDNF
          case (left, Or(l, r))         => ((left && l) || (left && r)).toDNF
          case (l, r)                   => l && r
        }
      case Not(p)             =>
        p.toDNF match {
          case And(left, right)   => (Not(left) || Not(right)).toDNF
          case Or(left, right)    => (Not(left) && Not(right)).toDNF
          case AlphaTest(_, _, _) => this
          case Not(p1)            => p1.toDNF
        }
      case Or(left, right)    => left.toDNF || right.toDNF
    }
  }
}

object Predicate {

  object IsAlpha {
    def unapply(p: Predicate): Option[(Predicate, Fact.Source)] = Option.when {
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

  final case class Test[T: FactOps] private (
    override val signature: String,
    test: T => Boolean,
    rep: Fact.Val[T]
  ) extends Predicate {
    override lazy val facts: Set[Fact[_]] = rep.facts
  }

  object Test {

    inline def apply[T: FactOps](rep: Fact[T], inline test: T => Boolean)(
      inline toSign: Any = test
    ): Test[T] = {
      Macros.createSigned[Test[T]](
        s => Test(s"${ rep.signature } $s", test, rep),
        toSign
      )
    }
  }

  final case class And(
    left: Predicate,
    right: Predicate
  ) extends Predicate {
    override lazy val facts: Set[Fact[_]] = left.facts ++ right.facts
    override val signature: String        = s"${ left.signature } && ${ right.signature }"
  }

  final case class Or(
    left: Predicate,
    right: Predicate
  ) extends Predicate {
    override lazy val facts: Set[Fact[_]] = left.facts ++ right.facts
    override val signature: String        = s"${ left.signature } || ${ right.signature }"
  }

  case class Not(p: Predicate) extends Predicate {
    override lazy val facts: Set[Fact[_]] = p.facts
    override val signature: String        = s"!${ p.signature }"
  }

  inline def apply[T1, T2](
    rep1: Fact[T1],
    rep2: Fact[T2],
    inline test: (T1, T2) => Boolean
  )(using
    TupleOps[T1 *: T2 *: EmptyTuple],
    NotTuple[T1],
    NotTuple[T2]
  ): AlphaTest[T1 *: T2 *: EmptyTuple] = ???

  private inline def createTuple[T <: NonEmptyTuple](
    rep: Fact[T],
    test: T => Boolean,
    inline sign: Any
  )(using T: TupleOps[T]): BetaTest[T] = {
    Macros.createSigned[BetaTest[T]](
      s => BetaTest(s"${ rep.signature } $s", test, rep.toVal),
      sign
    )
  }

  private inline def createAsTuple[T: NotTuple](
    rep: Fact[T],
    test: T => Boolean,
    inline sign: Any
  )(using T: FactOps[T]): BetaTest[T] = {
    Macros.createSigned[BetaTest[T]](
      s => BetaTest(s"${ rep.signature } $s", test, T.toVal(rep)),
      sign
    )
  }

  inline def fromTuple[T <: NonEmptyTuple : FactOps.TupleOps](
    rep: Fact.TMap[T],
    inline test: T => Boolean
  ): BetaTest[T] = ???

}
