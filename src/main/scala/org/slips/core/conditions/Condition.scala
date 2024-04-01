package org.slips.core.conditions

import org.slips.Env
import org.slips.Environment
import org.slips.NotTuple
import org.slips.Signature
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.BuildStep
import org.slips.core.fact.*
import org.slips.core.fact.Fact.Val
import org.slips.core.macros.Macros
import org.slips.core.network.Node
import org.slips.core.network.alpha.AlphaNode
import org.slips.syntax.*
import scala.annotation.targetName
import scala.compiletime.summonInline
import scala.util.NotGiven

sealed trait Condition[T] { self =>
  private[slips] val parse: ParseStep[T]
}

object Condition {

  inline def all[T : FactOps : NotTuple : ScalarFact]: All[T] =
    All[T](Signature.Manual(s"All[${ Macros.signType[T] }]"))

  sealed trait Source[T: FactOps](using ev: ScalarFact[T]) extends Condition[T] { self =>

    val signature: Signature

    override private[slips] val parse: ParseStep[T] = ParseStep
      .modify(_.addSource(self))
      .map[Fact.Val[T]](_ => ev.flip(fact))

    private def fact: Fact.Alpha.Source[T] = Fact.Alpha.Source(self)

    private[slips] def build: Env[BuildStep[Node]] = BuildStep.addNode(AlphaNode.Source(self.signature))
  }

  final case class All[T : FactOps : ScalarFact] private[Condition] (override val signature: Signature)
      extends Source[T]

  final case class Opaque[T](predicate: Predicate) extends Condition[Unit] {
    override private[slips] val parse: ParseStep[Unit] = // TODO: Move that to build package
      ParseStep.modify(_.addPredicate(predicate))
  }

  final case class Map[T, Q] private[slips] (src: Condition[T], f: Fact.Val[T] => Fact.Val[Q])
      extends Condition[Q] { self =>

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
  )(using T: FactOps[T]) extends Condition[T] {
    override private[slips] val parse: ParseStep[T] = for {
      t <- cond.parse
      predicate = f(t)
      _ <- Predicate.add(predicate.toDNF)
    } yield t
  }

}
