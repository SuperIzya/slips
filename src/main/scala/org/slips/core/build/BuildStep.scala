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
  def set(f: => BuildContext): BuildStep[Unit]                 = State.set(f)

  def getSourceNode[T](src: Condition.Source[T]): BuildStep[AlphaNode.Source[T]] =
    BuildStep(_.addSourceNode(src, AlphaNode.Source(src.signature)))

  def addAlphaNode[T](fact: Fact.Alpha[T], node: AlphaNode): BuildStep[AlphaNode] = {
    for {
      ctx <- BuildStep.get
      next  = ctx.nodes.getOrElse(node.signature, node)
      facts = ctx.nodeFacts.getOrElse(next, Set.empty)
      _ <- BuildStep.set {
        ctx.copy(
          nodes = ctx.nodes + (node.signature -> next),
          nodeFacts = ctx.nodeFacts + (next   -> facts + fact)
        )
      }
    } yield next
  }

  val get: BuildStep[BuildContext] = State.get[BuildContext]

  def addParsingResult(p: ParseResult): BuildStep[ParseResult] =
    State(s => s.addParsingResult(p) -> p)

  def pure[T](t: T): BuildStep[T] = State.pure(t)

}
