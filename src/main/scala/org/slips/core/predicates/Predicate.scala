package org.slips.core.predicates

import cats.Monoid
import org.slips.core.*
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Source
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Predicate extends Signed {
  def and(other: Predicate): Predicate = Predicate.And(this, other)
  def or(other: Predicate): Predicate  = Predicate.Or(this, other)
  def not: Predicate                   = Predicate.Not(this)

  @targetName("and_op")
  def &&(other: Predicate): Predicate = and(other)
  @targetName("or_op")
  def ||(other: Predicate): Predicate = or(other)

  override val signature: String = this.getClass.getSimpleName

  lazy val sources: Set[Fact[_]]

}

object Predicate {

  final case class Test[T](override val signature: String, test: T ⇒ Boolean, rep: Fact[T]) extends Predicate:
    override lazy val sources: Set[Fact[_]] = rep.sources.toSet

  object Test {

    private inline def create[T](
      rep: Fact[T],
      test: T ⇒ Boolean,
      inline sign: Any
    ): Test[T] = Macros.createSigned[Test[T]](
      s ⇒ Test(s"${ rep.signature } $s", test, rep),
      sign
    )

    inline def fromFact[T](rep: Fact[T], inline test: T ⇒ Boolean): Test[T] =
      create(rep, test, test)

    inline def apply[T1, T2](
      rep1: Fact[T1],
      rep2: Fact[T2],
      inline test: (T1, T2) ⇒ Boolean
    )(
      using TypeOps.TupleOps[(T1, T2)],
      Fact[T1] =:= Fact.Val[T1],
      Fact[T2] =:= Fact.Val[T2]
    ): Test[(T1, T2)] = {
      create(Fact.fromTuple(rep1 → rep2), test.tupled, test)
    }

    inline def fromTuple[T <: NonEmptyTuple : TypeOps.TupleOps](
      rep: Fact.TMap[T],
      inline test: T ⇒ Boolean
    ): Test[T] = create(Fact.fromTuple[T](rep), test, test)
  }

  final case class And(left: Predicate, right: Predicate) extends Predicate {
    override val signature: String = s"${ left.signature } && ${ right.signature }"

    override lazy val sources: Set[Fact[_]] = left.sources ++ right.sources
  }

  final case class Or(left: Predicate, right: Predicate) extends Predicate {
    override val signature: String          = s"${ left.signature } || ${ right.signature }"
    override lazy val sources: Set[Fact[_]] = left.sources ++ right.sources
  }

  final case class Not private (p: Predicate) extends Predicate {
    override val signature: String          = s"!${ p.signature }"
    override lazy val sources: Set[Fact[_]] = p.sources
  }

  object Not {
    def apply(p: Predicate)(using DummyImplicit): Predicate =
      p match
        case Not(pp) ⇒ pp
        case _       ⇒ new Not(p)
  }
}
