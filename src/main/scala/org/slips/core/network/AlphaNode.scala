package org.slips.core.network

import org.slips.core.build.BuildStep
import org.slips.core.conditions.Condition
import org.slips.core.network.materialized.Publisher
import org.slips.core.predicates.Predicate as TestPredicate

private[slips] sealed trait AlphaNode extends Node {
  override def materialize: BuildStep[materialized.Node] = ???
}

private[slips] object AlphaNode {

  case class Source[T](
    src: Condition.Source[T]
  ) extends AlphaNode {
    override def signature: String = src.signature
  }

  case class Predicate(
    p: TestPredicate,
    prev: AlphaNode
  ) extends AlphaNode {
    override def signature: String = s"${ prev.signature } -> ${ p.signature }"
  }

  case class Combine(
    left: AlphaNode,
    right: AlphaNode
  ) extends AlphaNode {
    override def signature: String = s"(${ left.signature }) && (${ right.signature })"
  }
}
