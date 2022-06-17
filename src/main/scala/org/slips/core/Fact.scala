package org.slips.core

import org.slips.core.conditions.{Condition, Predicate}
import org.slips.core.{Macros, Signature, Signed}

import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[T] extends Signed {
  override def signature: String = "_"

  inline def value[I](inline f: T => I): Fact[I] =
    Macros.createSigned(s => Fact.Map(s"$signature => $s", f, this), f)

  inline def test(inline f: T => Boolean): Predicate = Predicate.Test.fromFact(this, f)

  @targetName("repNotEq")
  def =!=(other: Fact[T]): Predicate = Predicate.Test(this, other, _ != _)

  @targetName("repEq")
  def ===(other: Fact[T]): Predicate = Predicate.Test(this, other, _ == _)

}

object Fact {

  type TMap = [x <: Tuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: Tuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped = [x <: Tuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match
    case NonEmptyTuple => TMap[X]
    case _ => Fact[X]

  final case class Tuples[T <: NonEmptyTuple] private[slips](override val signature: String, facts: TMap[T]) extends Fact[T]
  def fromTuple[T <: NonEmptyTuple](t: TMap[T])(using s: Signature[T]): Fact[T] =
    Tuples(s.extract(t).mkString("(", ", ", ")"), t)

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

  final case class Dummy[T] private[Fact](src: Condition[T]) extends Fact[T]:
    override val signature: String = s"${src.signature} -> Fact[${Macros.signType[T]}]"

  object Dummy {
    final case class FromTuple[T <: Tuple, Q] private[Fact](src: Condition[T], index: Int) extends Fact[Q]:
      override val signature: String = s"${src.signature} ~> ${Macros.signType[T]}($index) -> Fact[${Macros.signType[Q]}]"

    sealed trait Creator[T <: NonEmptyTuple] {
      protected val index: Int
      protected def chain[Q <: NonEmptyTuple](src: Condition[Q])(using s: Tuple.Size[Q]): Fact.TMap[T]
      def forSource(src: Condition[T])(using T <:< NonEmptyTuple, Tuple.Size[T]): Fact.Val[T] = chain[T](src)
    }
    object Creator {
      given [T]: Creator[T *: EmptyTuple] = new Creator[T *: EmptyTuple] {
        override protected val index: Int = 1

        override protected def chain[Q <: NonEmptyTuple](src: Condition[Q])
                                                        (using s: Tuple.Size[Q]): Fact.TMap[T *: EmptyTuple] =
          FromTuple[Q, T](src, s - index) *: EmptyTuple
      }

      given [T <: NonEmptyTuple, H](using prev: Creator[T]): Creator[H *: T] = new Creator[H *: T] {
        override protected val index: Int = prev.index + 1
        override protected def chain[Q <: NonEmptyTuple](src: Condition[Q])
                                                        (using s: Tuple.Size[Q]): Fact.TMap[H *: T] =
          FromTuple[Q, H](src, s - index) *: prev.chain(src)
      }
    }
  }

  def dummy[T <: NonEmptyTuple: Tuple.Size](src: Condition[T])(using C: Dummy.Creator[T]): Fact.Val[T] =
    C.forSource(src)

  inline def dummy[T](src: Condition[T])(using ev: Fact[T] =:= Val[T]): Fact.Val[T] = Dummy(src)

  private[slips] val unit: Fact[Unit] = new Fact[Unit] {}
}
