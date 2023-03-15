package org.slips.core.build

import cats.data.State
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.network.AlphaNode
import org.slips.core.network.Node
import org.slips.core.predicates.Predicate

object BuildStep {

  def apply[T](f: BuildContext => (BuildContext, T)): BuildStep[T] = State(f)

  def update(f: BuildContext => BuildContext): BuildStep[Unit] = State(f.andThen(_ -> ()))

  def addSourceNode[T](src: Condition.Source[T], node: => AlphaNode.Source[T]): BuildStep[AlphaNode.Source[T]] =
    BuildStep(_.addSourceNode(src, node))

  val get: BuildStep[BuildContext] = State.get[BuildContext]

  def addParsingResult(p: ParseResult): BuildStep[ParseResult] =
    State(s => s.addParsingResult(p) -> p)

  def pure[T](
    t: T
  ): BuildStep[T] = State.pure(t)

}
