package org.slips.core.conditions

import org.slips.Environment
import org.slips.core.*
import org.slips.core.Fact
import org.slips.core.builder.BuildStep
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Condition[T] extends Signed {
  def flatMap[Q](f: Fact.Val[T] ⇒ Condition[Q]): Condition.Res[Q] =
    Condition.FlatMap[T, Q](this, f)

  def map[R](f: Fact.Val[T] ⇒ Fact.Val[R]): Condition.Res[R] =
    Condition.mapFacts[T, R](this, f)
  /*
  @targetName("mapFacts")
  inline def map[R, Q <: NonEmptyTuple](f: Fact.TMap[Q] => Fact[R])
                                       (using T <:< NonEmptyTuple,
                                        T =:= Fact.TMap[Q]): Condition.Res[R] = ???*/

  def withFilter(f: Fact.Val[T] ⇒ Predicate): Condition.Res[T] =
    Condition.Filter(this, f)

  @targetName("withFilterSingle")
  def withFilter(f: Fact.Val[T] ⇒ Boolean): Condition.Res[T] =
    Condition.ScalarFilter(this, f)

  override val signature: String = ""

  private[slips] val build: BuildStep[T]
}

object Condition {
  type Res[x] = Environment ?=> Condition[x]

  private[slips] final case class FlatMap[T, Q](
    left: Condition[T],
    f: Fact.Val[T] ⇒ Condition[Q])
      extends Condition[Q] {
    override private[slips] val build: BuildStep[Q] =
      left.build.flatMap(f(_).build)
  }

  private[slips] final case class ScalarFilter[T](
    src: Condition[T],
    f: Fact.Val[T] ⇒ Boolean)
      extends Condition[T] {
    override private[slips] val build: BuildStep[T] = src
      .build
      .map {
        case x if f(x) ⇒ x
      }
  }

  private[slips] final case class Filter[T](
    cond: Condition[T],
    f: Fact.Val[T] ⇒ Predicate)
      extends Condition[T] {
    override private[slips] val build: BuildStep[T] = for {
      t ← cond.build
      _ ← BuildStep.modify(_.addPredicate(f(t)))
    } yield t
  }

  final case class Map[T, Q](
    override val signature: String,
    src: Condition[T],
    map: Fact.Val[T] ⇒ Fact.Val[Q])
      extends Condition[Q] {
    val build: BuildStep[Q] = src.build.map(map)
  }

  inline def map[T, Q](
    inline src: Condition[T],
    f: Fact.Val[T] ⇒ Fact.Val[Q]
  ): Map[T, Q] =
    Macros
      .createSigned[Map[T, Q]](s ⇒ Map(s"${ src.signature } => $s", src, f), f)

  sealed trait Source[T](private[slips] val fact: Fact.Val[T])
      extends Condition[T]:
    override private[slips] val build: BuildStep[T] = BuildStep.pure(fact)

  final case class All[T] private[Condition] (
    private[slips] override val fact: Fact.Val[T])
      extends Source[T](fact)
  def all[T : TypeOps : TypeOps.Size]: All[T] = {
    lazy val fact: Fact.Val[T] = summon[TypeOps[T]].forSource(res)
    lazy val res               = All[T](fact)
    res
  }

  final case class OpaquePredicate private[slips] (p: Predicate)
      extends Condition[Unit] {
    override val signature: String = p.signature

    override private[slips] val build: BuildStep[Unit] =
      BuildStep.modify(_.addPredicate(p))
  }

  final case class MapFacts[T, Q] private[Condition] (
    src: Condition[T],
    f: Fact.Val[T] ⇒ Fact.Val[Q])
      extends Condition[Q] {
    override val signature: String = ""

    override private[slips] val build: BuildStep[Q] = src
      .build
      .map {
        f(_) match {
          case f: Fact[Q]     ⇒ f.toVal
          case t: Fact.Val[Q] ⇒ t
        }
      }
  }
  private def mapFacts[T, Q](
    src: Condition[T],
    f: Fact.Val[T] ⇒ Fact.Val[Q]
  ): MapFacts[T, Q] =
    MapFacts(src, f)

}
