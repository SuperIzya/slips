package org.slips.core.network

import org.slips.core.build.BuildStep
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.network.materialized.Publisher
import org.slips.core.predicates.Predicate as TestPredicate

private[slips] sealed trait AlphaNode extends Node {

  def sourceNode: AlphaNode.Source[_]
  def facts: AlphaNode.Sources = Set.empty
}

private[slips] object AlphaNode {
  type Sources = Set[Fact.Source]

  /** Source node, producing values of type T */
  case class Source[T](override val signature: String) extends AlphaNode {
    override def sourceNode: Source[_] = this
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

    override def sourceNode: Source[_] = prev.sourceNode
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

    override def sourceNode: Source[_] = left.sourceNode
  }
}
