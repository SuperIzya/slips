package org.slips.core.network

import org.slips.Signature
import org.slips.core.build.BuildStep
import org.slips.core.conditions.{Predicate as TestPredicate, *}
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.network.materialized.Publisher

private[slips] sealed trait AlphaNode extends Node {

  val sourceNode: AlphaNode.Source[?]
}

private[slips] object AlphaNode {

  /** Source node, producing values of type T */
  case class Source[T](override val signature: Signature) extends AlphaNode { self =>
    override val sourceNode: Source[?] = self
  }

  /**
    * Alpha node that produces values satisfying to a
    * predicate
    */
  case class Predicate(
    p: TestPredicate,
    prev: AlphaNode
  ) extends AlphaNode {
    override val signature: Signature = prev.signature.unite(p)(_ + " -> " + _)

    override val sourceNode: Source[?] = prev.sourceNode
  }

  /**
    * Alpha node that produces a value when and only when it
    * is present both left and right
    */
  case class Combine(
    left: AlphaNode,
    right: AlphaNode
  ) extends AlphaNode {
    override val signature: Signature = left.signature.unite(right)(_ + " && " + _)

    override val sourceNode: Source[?] = left.sourceNode
  }
}
