package org.slips.core.predicates

import cats.Monoid
import cats.data.State
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core.*
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.ParseStep
import org.slips.core.fact.*
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Predicate extends Signed { self =>
  lazy val sourceFacts: Set[Fact.Source[_]] = facts.flatMap(_.sourceFacts)
  override def signature: Signature         = Signature.Manual(self.getClass.getSimpleName)

  def facts: Set[Fact[_]]

  def sources: Set[Condition.Source[_]] = facts.flatMap(_.sources)
}

object Predicate {

  def add(p: Predicate): ParseStep[Unit] = p match {
    case And(left, right) =>
      for {
        _ <- add(left)
        _ <- add(right)
      } yield Fact.unit
    case _                => ParseStep.modify(_.addPredicate(p))
  }

  final case class And(left: Predicate, right: Predicate) extends Predicate {
    override lazy val facts: Set[Fact[_]] = left.facts ++ right.facts

    override val signature: Signature = Signature.derivedBinary(left, right, (l, r) => s"$l && $r")

  }

  final case class Or(left: Predicate, right: Predicate) extends Predicate {
    override lazy val facts: Set[Fact[_]] = left.facts ++ right.facts

    override val signature: Signature = Signature.derivedBinary(left, right, (l, r) => s"$l || $r")
  }

  final case class NotExist[T : FactOps : ScalarFact](f: Fact[T]) extends Predicate {
    override lazy val facts: Set[Fact[_]] = f.predecessors + f
    override val signature: Signature     = Signature.derivedUnary(f, s => s"NotExist[$s]")

    def not: Predicate = Exist(f)
  }

  final case class Exist[T : FactOps : ScalarFact](f: Fact[T]) extends Predicate {
    override lazy val facts: Set[Fact[_]] = f.predecessors + f
    override val signature: Signature     = Signature.derivedUnary(f, s => s"Exist[$s]")

    def not: Predicate = NotExist(f)
  }

  final case class Not private (p: Predicate) extends Predicate {
    override lazy val facts: Set[Fact[_]] = p.facts
    override val signature: Signature     = Signature.derivedUnary(p, s => s"!($s)")
  }

  object Not {
    def apply(p: Predicate)(using DummyImplicit): Predicate =
      p match
        case Not(pp) => pp
        case _       => new Not(p)
  }

  final case class Test[T](
    override val signature: Signature,
    test: T => Boolean,
    rep: Fact.Val[T]
  )(using T: FactOps[T]
  ) extends Predicate {
    override lazy val facts: Set[Fact[_]] = T.predecessors(rep)

    def not: Test[T] = copy(
      signature = Signature.derivedUnary(this, s => s"!($s)"),
      test = (x: T) => !test(x)
    )
  }

  object Test {

    private[slips] inline def fromFact[T : NotTuple : FactOps](
      rep: Fact[T],
      inline test: T => Boolean
    )(using ev: ScalarFact[T]
    ): Test[T] =
      createSigned(ev.flip(rep), test, test)

    private[slips] inline def apply[T : ScalarFact : FactOps](
      rep1: Fact[T],
      rep2: Fact[T],
      inline test: (T, T) => Boolean
    )(using M: TupleFact[(T, T)]
    ): Test[(T, T)] =
      createSigned[(T, T)](M.flip(rep1 -> rep2), test.tupled, test)

    private[slips] inline def fromTuple[T <: NonEmptyTuple : FactOps.TupleOps](
      rep: Fact.Val[T],
      inline test: T => Boolean
    )(using
      ev: TupleFact[T]
    ): Test[T] =
      createSigned(ev.flip(rep), test, test)

    private[slips] inline def createSigned[T](
      rep: Fact.Val[T],
      test: T => Boolean,
      inline sign: Any
    )(using T: FactOps[T]
    ): Test[T] =
      Macros.createSigned[Test[T]](
        s => Test(Signature.derivedUnary(T.extract(rep).toSignature, r => s"$r $s"), test, rep),
        sign
      )
  }
}
