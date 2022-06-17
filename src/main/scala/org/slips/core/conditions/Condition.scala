package org.slips.core.conditions

import org.slips.core.*
import org.slips.Environment
import org.slips.core.Fact

import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Condition[T] extends Signed {
  def flatMap[Q](f: Fact.Val[T] => Condition[Q]): Condition.Res[Q] =
    Condition.FlatMap[T, Q](this, f)

  def map[R](f: Fact.Val[T] => Fact[R]): Condition.Res[R] =
    Condition.mapToFact[T, R](this, f)
/*
  @targetName("mapFacts")
  inline def map[R, Q <: NonEmptyTuple](f: Fact.TMap[Q] => Fact[R])
                                       (using T <:< NonEmptyTuple,
                                        T =:= Fact.TMap[Q]): Condition.Res[R] = ???*/

  def withFilter(f: Fact.Val[T] => Predicate): Condition.Res[T] =
    Condition.Filter(this, f)

  @targetName("withFilterSingle")
  def withFilter(f: Fact.Val[T] => Boolean): Condition.Res[T] = this

  override val signature: String = ""
}
object Condition {
  type Res[x] = Environment ?=> Condition[x]

  private[slips] final case class FlatMap[T, Q](left: Condition[T], f: Fact.Val[T] => Condition[Q])
    extends Condition[Q]

  private[slips] final case class ScalarFilter[T](src: Condition[T], f: Fact.Val[T] => Boolean) extends Condition[T]

  private[slips] final case class Filter[T](cond: Condition[T], f: Fact.Val[T] => Predicate)
    extends Condition[T]

  final case class Map[T, Q](override val signature: String, src: Condition[T], f: Fact[T] => Q) extends Condition[Q]
  inline def map[T, Q](inline src: Condition[T], f: Fact[T] => Q): Map[T, Q] =
    Macros.createSigned(s => Map(s"${src.signature} => $s", src, f), f)

  sealed trait Source[T] extends Condition[T]

  final case class All[T] private[slips]() extends Source[T]

  final case class Collector[T <: NonEmptyTuple](src: Fact.TMap[T])(using s: Signature[T]) extends Condition[T]:
    override val signature: String = s.extract(src).mkString("(", ", ", ")")

  final case class OpaquePredicate(p: Predicate) extends Condition[Unit] {
    override val signature: String = p.signature
  }

  final case class MapToFact[T, Q](src: Condition[T], f: Fact.Val[T] => Fact[Q])
    extends Condition[Q] {
    override val signature: String = ""
  }
  def mapToFact[T, Q](src: Condition[T], f: Fact.Val[T] => Fact[Q]): MapToFact[T, Q] =
    MapToFact(src, f)

  final case class MapToFacts[T, Q <: NonEmptyTuple](override val signature: String,
                                                     src: Condition[T],
                                                     f: Fact[T] => Fact.TMap[Q])
    extends Condition[Q]
  inline def mapToFacts[T, Q <: NonEmptyTuple](inline src: Condition[T], f: Fact[T] => Fact.TMap[Q], sign: Any): MapToFacts[T, Q] =
    Macros.createSigned(s => MapToFacts(s"${src.signature} => $s", src, f), sign)
}
