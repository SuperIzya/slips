package org.slips.core.build

import cats.data.State
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.predicates.Predicate

object BuildStep {

  def addNode(
    node: Node
  ): BuildStep[Node] = State(s => s.addNode(node))

  def apply[T](
    f: BuildContext => (
      BuildContext,
      T
    )
  ): BuildStep[T] = State(f)

  def update(
    f: BuildContext => BuildContext
  ): BuildStep[Unit] = State(f.andThen(_ -> ()))

  def addSource[T](
    src: Condition.Source[T]
  ): BuildStep[Unit] = State(s => s.addSource(src) -> ())

  def addParsingResult(
    p: ParseResult
  ): BuildStep[ParseResult] =
    State(s => s.addParsingResult(p) -> p)

  def pure[T](
    t: T
  ): BuildStep[T] = State.pure(t)

}
