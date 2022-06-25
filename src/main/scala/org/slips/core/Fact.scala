package org.slips.core

import TypeOps.TupleOps
import cats.Monoid
import org.slips.core.Fact.Tuples
import org.slips.core.Macros
import org.slips.core.Signed
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Predicate
import scala.Tuple.Size
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Fact[T](val sample: T)(using O: TypeOps[T]) extends Signed {

  override def signature: String = s"${ Macros.signType[this.type] }[${ Macros.signType[T] }]($sample)"

  inline def value[I: TypeOps](inline f: T ⇒ I): Fact[I] =
    Macros.createSigned[Fact.Map[T, I]](
      s ⇒ Fact.Map(s"$signature => $s", f, this),
      f
    )

  inline def test(inline f: T ⇒ Boolean): Predicate = Predicate.Test.fromFact(this, f)

  @targetName("repNotEq")
  inline def =!=(other: Fact[T])(using TupleOps[(T, T)], Fact[T] =:= Fact.Val[T]): Predicate =
    Predicate.Test(this, other, _ != _)

  @targetName("repEq")
  inline def ===(other: Fact[T])(using TupleOps[(T, T)], Fact[T] =:= Fact.Val[T]): Predicate =
    Predicate.Test(this, other, _ == _)

  lazy val toVal: Fact.Val[T] = O.toVal(this)
}

object Fact {

  type TMap        = [x <: Tuple] =>> Tuple.Map[x, Fact]
  type TInverseMap = [x <: Tuple] =>> Tuple.InverseMap[x, Fact]
  type TIsMapped   = [x <: Tuple] =>> Tuple.IsMappedBy[Fact][x]

  type Val[X] = X match
    case Tuple ⇒ TMap[X]
    case _     ⇒ Fact[X]

  type ReverseVal[X] = X match
    case Tuple   ⇒ TInverseMap[X]
    case Fact[x] ⇒ x

  final case class ExtractFromTuple[T <: NonEmptyTuple, Q: TypeOps] private[slips] (
    override val signature: String,
    src: Fact[T],
    extract: T ⇒ Q,
    override val sample: Q)
      extends Fact[Q](sample)

  final case class Tuples[T <: NonEmptyTuple : TupleOps] private[slips] (
    override val signature: String,
    facts: TMap[T],
    override val sample: T)
      extends Fact[T](sample)

  def fromFactTuple[T <: NonEmptyTuple, Q](
    f: Fact[T],
    extract: T ⇒ Q,
    index: Int
  )(
    using Q: TypeOps[Q],
    T: TypeOps.Size[T]
  ): Fact[Q] = ExtractFromTuple(
    s"${ f.signature }($index of ${ T.size })[${ Q.signature }]",
    f,
    extract,
    Q.empty
  )

  def fromTuple[T <: NonEmptyTuple](t: TMap[T])(using T: TypeOps.TupleOps[T]): Fact[T] =
    Tuples(
      T.extract(t).mkString("(", ", ", ")"),
      t,
      T.empty
    )

  final case class Map[T, Q: TypeOps](
    override val signature: String,
    f: T ⇒ Q,
    rep: Fact[T])
      extends Fact[Q](f(rep.sample))

  sealed trait CanBeLiteral[T]
  object CanBeLiteral {
    given [T <: Tuple](
      using NotGiven[TIsMapped[T]]
    ): CanBeLiteral[T] =
      new CanBeLiteral[T] {}

    given [T](
      using NotGiven[T <:< Tuple],
      NotGiven[T =:= Fact[?]]
    ): CanBeLiteral[T] =
      new CanBeLiteral[T] {}
  }

  final case class Literal[I: TypeOps] private[slips] (value: I)
      extends Fact[I](value) {
    override lazy val signature: String = value.toString
  }
  def literal[T : CanBeLiteral : TypeOps](v: T): Fact[T] = Literal(v)

  final case class Dummy[T: TypeOps] private[slips] (
    src: Condition[T],
    override val sample: T)
      extends Fact[T](sample) {
    override val signature: String =
      s"${ src.signature } -> Fact[${ Macros.signType[T] }]"
  }

  final case class FromTuple[T <: Tuple, Q: TypeOps] private[slips] (
    src: Condition[T],
    index: Int,
    override val sample: Q)
      extends Fact[Q](sample) {
    override val signature: String =
      s"${ src.signature } ~> ${ Macros.signType[T] }($index) -> Fact[${ Macros.signType[Q] }]"
  }

  def dummy[T <: NonEmptyTuple](src: Condition[T])(using T: TypeOps[T]): Fact.Val[T] =
    T.forSource(src)

  inline def dummy[T: TypeOps](
    src: Condition[T]
  )(
    using ev: Fact[T] =:= Val[T],
    T: Monoid[T]
  ): Fact.Val[T] = Dummy(src, T.empty)

  private[slips] val unit: Fact[Unit] = new Fact[Unit](()) {}
}
