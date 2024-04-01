package org.slips.core.build

import org.slips.Env
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.network.alpha.AlphaNode
import org.slips.syntax.*

trait NodeBuilder[T] {
  def alphaNode(n: T): Env[Option[BuildStep[AlphaNode]]] = None
}

object NodeBuilder {

  extension [T](t: T) {
    def alphaNode(using T: NodeBuilder[T]): Env[Option[BuildStep[AlphaNode]]] = {
      T.alphaNode(t)
    }
  }
  /*
  given [T]: NodeBuilder[Source[T]] = new NodeBuilder[Source[T]] {
    override def alphaNode(n: Source[T]): Env[Option[BuildStep[AlphaNode]]] = Some(BuildStep.getSourceNode(n))
  }*/

  given [T]: NodeBuilder[Test[T]] = new NodeBuilder[Test[T]] {
    override def alphaNode(n: Test[T]): Env[Option[BuildStep[AlphaNode]]] = Option
      .when(n.facts.forall(_.isAlpha) && n.facts.flatMap(_.alphaSources).size == 1) {
        /// BuildStep.addAlphaNode(n.rep.source, AlphaNode.Predicate(n, src))
        ???
      }
  }

  given [T <: Predicate]: NodeBuilder[T] = new NodeBuilder[T] {}

}
