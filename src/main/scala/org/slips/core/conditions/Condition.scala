package org.slips.core.conditions

import org.slips.Environment
import org.slips.core.*
import org.slips.core.predicates.Predicate
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Condition[T] extends Signed {
  override val signature: String = ""
  private[slips] val parse: ParseStep[T]

  def flatMap[Q](f: Fact.Val[T] => Condition[Q]): Condition.Res[Q] =
    Condition.FlatMap[T, Q](this, f)

  def map[R, Q](f: Fact.Val[T] => R)(using ev: Fact.ReverseVal[R] =:= Q, ev2: R =:= Fact.Val[Q]): Condition.Res[Q] =
    Condition.map[T, Q](this, f.andThen(ev2(_)))

  def withFilter(f: Fact.Val[T] => Predicate): Condition.Res[T] =
    Condition.Filter(this, f)

  @targetName("withFilterSingle")
  def withFilter(f: Fact.Val[T] => Boolean): Condition.Res[T] =
    Condition.ScalarFilter(this, f)
}

object Condition {
  type Res[x] = Environment ?=> Condition[x]

  inline def all[T : TypeOps : TypeOps.Size]: All[T] = {
    val signature: String = s"All[${ Macros.signType[T] }]"
    val fact: Fact.Val[T] = Fact.Source[T](signature).toVal
    val res               = All[T](signature, fact)
    res
  }

  private def map[T, Q](src: Condition[T], f: Fact.Val[T] => Fact.Val[Q]): Map[T, Q] =
    Map(src, f)

  sealed trait Source[T](private[slips] val fact: Fact.Val[T])
      extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = ParseStep.modify(_.addSource(this)).map(_ => fact)
  }

  final case class All[T] private[Condition] (
    override val signature: String,
    private[slips] override val fact: Fact.Val[T]
  )(
    using T: TypeOps[T]) extends Source[T](fact)

  final case class OpaquePredicate private[slips] (p: Predicate)
      extends Condition[Unit] {
    override val signature: String = p.signature

    override private[slips] val parse: ParseStep[Unit] = Predicate.add(p.toKNF)

  }

  final case class Map[T, Q] private[Condition] (
    src: Condition[T],
    f: Fact.Val[T] => Fact.Val[Q])
      extends Condition[Q] {
    override val signature: String = ""

    override private[slips] val parse: ParseStep[Q] = src.parse.map(f)
  }

  private[slips] final case class FlatMap[T, Q](
    left: Condition[T],
    f: Fact.Val[T] => Condition[Q])
      extends Condition[Q] {
    override private[slips] val parse: ParseStep[Q] =
      left.parse.flatMap(f(_).parse)
  }

  private[slips] final case class ScalarFilter[T](
    src: Condition[T],
    f: Fact.Val[T] => Boolean)
      extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = src
      .parse
      .map {
        case x if f(x) => x
      }
  }

  private[slips] final case class Filter[T](
    cond: Condition[T],
    f: Fact.Val[T] => Predicate)
      extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = for {
      t <- cond.parse
      _ <- ParseStep.modify(_.addPredicate(f(t)))
    } yield t
  }

}
