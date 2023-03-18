package org.slips.core.build

import org.slips.core.conditions.Condition
import org.slips.core.network.AlphaNode
import org.slips.core.predicates.Predicate

trait NodeBuilder[T] {
  def alphaNode(n: T): Option[BuildStep[AlphaNode]] = None
}

object NodeBuilder {

  given [T]: NodeBuilder[Condition.Source[T]] = new NodeBuilder[Condition.Source[T]] {
    def alphaNode(n: Condition.Source[T]): Option[BuildStep[AlphaNode.Source[T]]] = Some(BuildStep.getSourceNode(n))
  }

  given [T]: NodeBuilder[Predicate.Test[T]] = new NodeBuilder[Predicate.Test[T]] {
    def alphaNode(n: Predicate.Test[T]): Option[BuildStep[AlphaNode]] = Option
      .when(n.facts.forall(_.isAlpha) && n.facts.flatMap(_.alphaSources).size == 1) {
        BuildStep.addAlphaNode(n.rep.source, AlphaNode.Predicate(n, src))
      }
  }

  given [T <: Predicate]: NodeBuilder[T] = new NodeBuilder[T] {}

}
