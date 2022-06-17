package org.slips.core.conditions

import org.slips.core.Fact
import org.slips.core.*
import org.slips.core.conditions.Condition.Source

import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Predicate extends Signed {
  def and (other: Predicate): Predicate = Predicate.And(this, other)
  def or (other: Predicate): Predicate = Predicate.Or(this, other)
  def not: Predicate = Predicate.Not(this)

  @targetName("and_op")
  def && (other: Predicate): Predicate = and(other)
  @targetName("or_op")
  def || (other: Predicate): Predicate = or(other)

  override val signature: String = this.getClass.getSimpleName

}

object Predicate {

  final case class Test[T](override val signature: String, test: T => Boolean, rep: Fact[T]) extends Predicate

  object Test {

    private inline def create[T](rep: Fact[T], test: T => Boolean, inline sign: Any): Test[T] =
      Macros.createSigned(s => Test(s"${rep.signature} $s", test, rep), sign)

    inline def fromFact[T](rep: Fact[T], inline test: T => Boolean): Test[T] =
      create(rep, test, test)

    inline def apply[T1, T2](rep1: Fact[T1], rep2: Fact[T2], inline test: (T1, T2) => Boolean): Test[(T1, T2)] =
      create(Fact.fromTuple(rep1, rep2), { case (t1, t2) => test(t1, t2)}, test)

    inline def fromTuple[T <: NonEmptyTuple: Signature](rep: Fact.TMap[T], inline test: T => Boolean): Test[T] =
      create(Fact.fromTuple(rep), test, test)
  }

  final case class FilterMany[Q <: NonEmptyTuple](
    src: Condition[Q],
    f: Fact.TMap[Q] => Predicate
  ) extends Predicate

  final case class And(left: Predicate, right: Predicate) extends Predicate
  final case class Or(left: Predicate, right: Predicate) extends Predicate
  final case class Not private(p: Predicate) extends Predicate {
    override val signature: String = s"!${p.signature}"
  }
  object Not {
    def apply(p: Predicate)(using DummyImplicit): Predicate = p match
      case Not(pp) => pp
      case _ => new Not(p)
  }
}
