package org.slips.core.network.alpha

import org.slips.Signature
import org.slips.core.build.BuildStep
import org.slips.core.conditions.{Predicate as TestPredicate, *}
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.network.materialized.Publisher

private[slips] sealed trait AlphaNode extends Node {

  def sourceNode: AlphaNode.Source[?]
}

private[slips] object AlphaNode {

  /** Source node, producing values of type T */
  case class Source[T](override val signature: String) extends AlphaNode {
    override def sourceNode: Source[?] = this
  }

  /**
    * Alpha node that produces values satisfying to a
    * predicate
    */
  case class Predicate(
    p: TestPredicate,
    prev: AlphaNode
  ) extends AlphaNode {
    override def signature: String = s"${ prev.signature } -> ${ p.signature }"

    override def sourceNode: Source[?] = prev.sourceNode
  }

  /**
    * Alpha node that produces a value when and only when it
    * is present both left and right
    */
  case class Combine(
    left: AlphaNode,
    right: AlphaNode
  ) extends AlphaNode {
    override def signature: String = s"(${ left.signature }) && (${ right.signature })"

    override def sourceNode: Source[?] = left.sourceNode
  }
}
