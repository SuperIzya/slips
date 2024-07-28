package org.slips.core.network

import org.slips.Signature
import org.slips.core.build.EnvBuildStep
import org.slips.core.conditions.{Predicate as TestPredicate, *}
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.network.materialized.Publisher

private[slips] sealed trait AlphaNode[F[_]] extends Node[F] {

  val sourceNode: AlphaNode.Source[F, ?]
}

private[slips] object AlphaNode {

  /** Source node, producing values of type T */
  case class Source[F[_], T](override val signature: Signature) extends AlphaNode[F] { self =>
    override val sourceNode: Source[F, ?] = self
  }

  /**
    * Alpha node that produces values satisfying to a
    * predicate
    */
  case class Predicate[F[_]](p: TestPredicate, prev: AlphaNode[F]) extends AlphaNode[F] {
    override val signature: Signature = prev.signature.unite(p)(_ + " -> " + _)

    override val sourceNode: Source[F, ?] = prev.sourceNode
  }

  /**
    * Alpha node that produces a value when and only when it
    * is present both left and right
    */
  case class Combine[F[_]](left: AlphaNode[F], right: AlphaNode[F]) extends AlphaNode[F] {
    override val signature: Signature = left.signature.unite(right)(_ + " && " + _)

    override val sourceNode: Source[F, ?] = left.sourceNode
  }
}
