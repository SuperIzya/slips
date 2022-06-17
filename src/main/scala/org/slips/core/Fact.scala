package org.slips.core

import org.slips.core.conditions.Predicate
import org.slips.core.Fact.Collection.{Append, Collect, Concat, Empty}
import org.slips.core.Fact.{Collection, fromTuple, prepend}
import org.slips.core.{Macros, Signature, Signed}

import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[T] extends Signed {
  override def signature: String = "_"

  inline def value[I](inline f: T => I): Fact[I] = Fact.map(this, f)

  inline def test(inline f: T => Boolean): Predicate = Predicate.Test.fromFact(this, f)

  @targetName("repNotEq")
  def =!=(other: Fact[T]): Predicate = Predicate.Test(this, other, _ != _)

  @targetName("repEq")
  def ===(other: Fact[T]): Predicate = Predicate.Test(this, other, _ == _)

  @targetName("append")
  def *:[Q](other: Fact[Q])(using NotGiven[T <:< Tuple], NotGiven[Q <:< Tuple]): Collection[(T, Q)] =
    fromTuple(this, other)

  @targetName("appendCol")
  def *:[Q <: NonEmptyTuple](other: Collection[Q])(using NotGiven[T <:< Tuple]): Collection[T *: Q] =
    prepend(this, other)
}

object Fact {

  type TMap = [x <: Tuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: Tuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped = [x <: Tuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match
    case NonEmptyTuple => TMap[X]
    case _ => Fact[X]

  def unapply[T <: NonEmptyTuple](f: Fact[T]): Option[TMap[T]] = f match {
    case c: Fact.Collection[T] => Some(c.collect)
    case _ => None
  }

  inline def map[T, Q](inline src: Fact[T], f: T => Q): Map[T, Q] =
    Macros.createSigned(s => Map(s"${src.signature} => $s", f, src), f)

  def concat[T <: NonEmptyTuple, Q <: NonEmptyTuple](first: Collection[T], second: Collection[Q]): Collection[Tuple.Concat[T, Q]] = {
    val signature = s"${first.signature} *: ${second.signature}"
    Concat(signature, first, second)
  }

  def append[X, T <: NonEmptyTuple](head: Collection[T], tail: Fact[X])
                                   (using NotGiven[X <:< Tuple]): Collection[Tuple.Concat[T, X *: EmptyTuple]] = {
    val signature = s"${head.signature} *: ${tail.signature}"
    Concat(signature, head, single(tail))
  }

  def single[T](head: Fact[T])(using NotGiven[T <:< Tuple]): Collection[T *: EmptyTuple] =
    Append(s"(${head.signature})", head, Empty)

  def prepend[X, T <: NonEmptyTuple](head: Fact[X], tail: Collection[T])(using NotGiven[X <:< Tuple]): Collection[X *: T] =
    Append(s"${head.signature} *: ${tail.signature}", head, tail)

  def fromTuple[T <: NonEmptyTuple : Signature](reps: TMap[T]): Collection[T] = {
    val sign: String = Signature[T].extract(reps).mkString("(", ",", ")")
    Collect[T](sign, reps)
  }

  final case class Map[T, Q](override val signature: String, f: T => Q, rep: Fact[T]) extends Fact[Q]

  sealed trait CanBeLiteral[T]
  object CanBeLiteral {
    given[T <: Tuple](using NotGiven[TIsMapped[T]]): CanBeLiteral[T] = new CanBeLiteral[T] {}

    given[T](using NotGiven[T <:< Tuple], NotGiven[Fact[T]]): CanBeLiteral[T] = new CanBeLiteral[T] {}
  }

  final case class Literal[I] private[Fact] (valueFn: () => I) extends Fact[I] {
    lazy val value: I = valueFn()
    override lazy val signature: String = value.toString
  }
  def literal[T: CanBeLiteral](v: => T): Fact[T] = Literal(() => v)

  sealed trait Collection[T <: Tuple] extends Fact[T]:
    def collect: Fact.TMap[T] = ???

  object Collection {
    final case class Collect[T <: NonEmptyTuple](
      override val signature: String,
      collection: TMap[T]
    ) extends Collection[T]

    final case class Append[T <: NonEmptyTuple](
      override val signature: String,
      head: Fact[Tuple.Head[T]],
      tail: Collection[Tuple.Tail[T]]
    ) extends Collection[T]

    final case class Concat[T <: NonEmptyTuple, Q <: NonEmptyTuple](
      override val signature: String,
      first: Collection[T],
      second: Collection[Q]
    ) extends Collection[Tuple.Concat[T, Q]]

    case object Empty extends Collection[EmptyTuple] {
      override val signature: String = ""
    }

  }

}
