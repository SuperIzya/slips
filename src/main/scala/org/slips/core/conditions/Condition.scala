package org.slips.core.conditions

import org.slips.Environment
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.BuildStep
import org.slips.core.fact.*
import org.slips.core.fact.Fact.Val
import org.slips.core.network.AlphaNode
import org.slips.core.network.Node
import org.slips.core.predicates.Predicate
import org.slips.syntax.*
import scala.annotation.targetName
import scala.util.NotGiven

sealed trait Condition[T] extends Signed { self =>

  override val signature: Signature = Signature.Manual("")
  private[slips] val parse: ParseStep[T]
}

object Condition {
  type Res[x] = Environment ?=> Condition[x]

  inline def all[T : FactOps : ScalarFact]: All[T] =
    All[T](Signature.Manual(s"All[${ Macros.signType[T] }]"))

  sealed trait Source[T: FactOps](using ev: ScalarFact[T]) extends Condition[T] { self =>

    override private[slips] val parse: ParseStep[T] = ParseStep
      .modify(_.addSource(self))
      .map[Fact.Val[T]](_ => ev.flip(fact))

    private def fact: Fact.Source[T] = Fact.Source(self)

    private[slips] def build: BuildStep[Node] = BuildStep.addNode(AlphaNode.Source(self))
  }

  sealed trait Mappable[A] {
    type Out <: Condition[_]
    def map[T](condition: Condition[T], f: Fact.Val[T] => A): Out
  }

  final case class All[T : FactOps : ScalarFact] private[Condition] (override val signature: Signature)
      extends Source[T]

  final case class Map[T, Q] private[slips] (src: Condition[T], f: Fact.Val[T] => Fact.Val[Q])
      extends Condition[Q] { self =>

    override val signature: Signature = Signature.Manual("")

    override private[slips] val parse: ParseStep[Q] = src.parse.map(f)

  }

  private[slips] final case class FlatMap[T, Q](left: Condition[T], f: Fact.Val[T] => Condition[Q])
      extends Condition[Q] { self =>
    override private[slips] val parse: ParseStep[Q] =
      left.parse.flatMap(f(_).parse)

  }

  private[slips] final case class Filter[T](
    cond: Condition[T],
    f: Fact.Val[T] => Predicate
  ) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = for {
      t <- cond.parse
      predicate = f(t)
      _ <- Predicate.add(predicate.toKNF)
    } yield t
  }

  object Mappable {

    given step[H, L <: NonEmptyTuple](using evH: ScalarFact[H], prev: Mappable[Fact.Val[L]]): Mappable[Fact.Val[H *: L]]
    with {
      override type Out = Condition[H *: L]

      override def map[T](condition: Condition[T], f: Val[T] => Fact.Val[H *: L]): Out =
        Map(condition, f)
    }

  }
}
