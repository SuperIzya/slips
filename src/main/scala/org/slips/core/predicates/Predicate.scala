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
  type SourceFact <: Fact[_]
  type Facts <: Set[_ <: Fact[_]]

  lazy val sourceFacts: Set[SourceFact] = facts.flatMap(_.sourceFacts)
  override val signature: String        = this.getClass.getSimpleName

  def buildAlphaNode: Option[AlphaNode => AlphaNode] = None

  def facts: Facts

  def sources: Set[Condition.Source[_]] = facts.flatMap(_.sources)

  def and(other: Predicate): Predicate = Predicate.And(this, other)

  def or(other: Predicate): Predicate = Predicate.Or(this, other)

  @targetName("and_op")
  def &&(other: Predicate): Predicate = and(other)

  @targetName("or_op")
  def ||(other: Predicate): Predicate = or(other)

  @targetName("not_op")
  def unary_! : Predicate = Predicate.Not(this)

  def toKNF: Predicate = {
    import Predicate.*
    this match {
      case AlphaTest(_, _, _) => this
      case And(left, right)   => left.toKNF && right.toKNF
      case Not(p)             =>
        p.toKNF match {
          case And(left, right)   => (Not(left) || Not(right)).toKNF
          case Or(left, right)    => (Not(left) && Not(right)).toKNF
          case AlphaTest(_, _, _) => this
          case Not(p1)            => p1.toKNF
        }
      case Or(left, right)    =>
        (left.toKNF, right.toKNF) match {
          case (And(l, r), p) => (l.toKNF || p.toKNF).toKNF && (r.toKNF || p.toKNF).toKNF
          case (Or(l, r), p)  => (l.toKNF || r.toKNF || p.toKNF).toKNF
          case (p, And(l, r)) => (p.toKNF || l.toKNF).toKNF && (p.toKNF || r.toKNF).toKNF
          case (p, Or(l, r))  => (p.toKNF || l.toKNF || r.toKNF).toKNF
          case (l, r)         => l || r
        }
    }
  }
}

object Predicate {

  sealed trait Alpha extends Predicate {
    override type SourceFact = Fact.Source
    override type Facts      = Set[Fact[_]]
  }

  sealed trait Beta extends Predicate {
    override type SourceFact = Fact[_]
    override type Facts      = Set[Fact[_]]
  }
  def add(p: Predicate): ParseStep[Unit] = p match {
    case And(left, right) =>
      for {
        _ <- add(left)
        _ <- add(right)
      } yield Fact.unit
    case _                => ParseStep.modify(_.addPredicate(p))
  }

  final case class AlphaTest[T](
    override val signature: String,
    test: T => Boolean,
    rep: Fact.Alpha[T]
  ) extends Alpha {
    override lazy val facts: Set[Fact[_]] = rep.predecessors

    override def buildAlphaNode: Option[AlphaNode => AlphaNode] = Option.when(facts.size == 1) { prev =>
      AlphaNode.Predicate(this, prev)
    }

  }

  final case class BetaTest[T](
    override val signature: String,
    test: T => Boolean,
    rep: Fact.Val[T]
  )(using T: FactOps[T]) extends Beta {
    override lazy val facts: Set[Fact[_]] = rep.toFacts

  }

  final case class And(
    left: Predicate,
    right: Predicate
  ) extends Beta {
    override lazy val facts: Set[Fact[_]] = left.facts ++ right.facts
    override val signature: String        = s"${ left.signature } && ${ right.signature }"
  }

  final case class Or(
    left: Predicate,
    right: Predicate
  ) extends Beta {
    override lazy val facts: Set[Fact[_]] = left.facts ++ right.facts
    override val signature: String        = s"${ left.signature } || ${ right.signature }"
  }

  sealed trait Not { this: Predicate =>
    val p: Predicate
    override lazy val facts: p.Facts = p.facts
    override val signature: String   = s"!${ p.signature }"
  }
  object Not       {

    def unapply(p: Predicate): Option[Predicate] = {
      p match {
        case NotAlpha(a) => Some(a)
        case NotBeta(b)  => Some(b)
        case _           => None
      }
    }

    private case class NotAlpha(p: Predicate.Alpha) extends Alpha with Not

    private case class NotBeta(p: Predicate.Beta) extends Beta with Not

    def apply(p: Predicate): Predicate = {
      p match {
        case alpha: Alpha =>
          alpha match {
            case NotAlpha(pp) => pp
            case _            => NotAlpha(alpha)
          }
        case beta: Beta   =>
          beta match {
            case NotBeta(p) => p
            case _          => NotBeta(beta)
          }
      }
    }

  }
  object AlphaTest {

    inline def fromScalar[T: FactOps](rep: Fact.Alpha[T], inline test: T => Boolean): AlphaTest[T] = {
      createScalar(rep, test, test)
    }

    private inline def createScalar[T](
      rep: Fact.Alpha[T],
      test: T => Boolean,
      inline sign: Any
    ): AlphaTest[T] = Macros.createSigned[AlphaTest[T]](
      s => AlphaTest(s"${ rep.signature } $s", test, rep),
      sign
    )
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
