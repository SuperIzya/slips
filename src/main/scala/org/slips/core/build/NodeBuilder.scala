package org.slips.core.build

import org.slips.Env
import org.slips.core.conditions.Condition
import org.slips.core.network.alpha.AlphaNode
import org.slips.core.predicates.Predicate

trait NodeBuilder[T] {
  def alphaNode(n: T): Env[Option[BuildStep[AlphaNode]]] = None
}

object NodeBuilder {

  extension [T](t: T) {
    def alphaNode(using T: NodeBuilder[T]): Env[Option[BuildStep[AlphaNode]]] = {
      T.alphaNode(t)
    }
  }

  given [T]: NodeBuilder[Condition.Source[T]] = new NodeBuilder[Condition.Source[T]] {
    override def alphaNode(n: Condition.Source[T]): Env[Option[BuildStep[AlphaNode]]] = Some(BuildStep.getSourceNode(n))
  }

  given [T]: NodeBuilder[Predicate.Test[T]] = new NodeBuilder[Predicate.Test[T]] {
    override def alphaNode(n: Predicate.Test[T]): Env[Option[BuildStep[AlphaNode]]] = Option
      .when(n.facts.forall(_.isAlpha) && n.facts.flatMap(_.alphaSources).size == 1) {
        /// BuildStep.addAlphaNode(n.rep.source, AlphaNode.Predicate(n, src))
        ???
      }
  }

  given [T <: Predicate]: NodeBuilder[T] = new NodeBuilder[T] {}

}
