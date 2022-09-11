package org.slips.core.build.strategy

import org.slips.core.fact.Fact
import org.slips.core.network.AlphaNetwork
import org.slips.core.network.AlphaNode
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec

trait AlphaNodeStrategy {
  def fold(network: AlphaNetwork.Intermediate, pair: (Predicate, Set[Fact.Source[_]])): AlphaNetwork.Intermediate =
    addAlphaNode(network, pair._1, pair._2)

  def addAlphaNode(
    network: AlphaNetwork.Intermediate,
    predicate: Predicate,
    facts: Set[Fact.Source[_]]
  ): AlphaNetwork.Intermediate
}

object AlphaNodeStrategy {
  object MaximumUtil extends AlphaNodeStrategy {
    override def addAlphaNode(
      network: AlphaNetwork.Intermediate,
      predicate: Predicate,
      facts: Set[Fact.Source[_]]
    ): AlphaNetwork.Intermediate = {
      @tailrec
      def add(tFacts: Set[Fact.Source[_]], n: AlphaNetwork.Intermediate): AlphaNetwork.Intermediate = {
        if (tFacts.isEmpty) n
        else {
          network.topNodes.map { case (f, n) => facts.intersect(f) -> n }.sortBy(-_._1.size).headOption match {
            case Some((f, prev)) =>
              val node = AlphaNode.Predicate(predicate, prev)
              add(tFacts -- f, n.copy(topNodes = (f -> node) +: n.topNodes))
            case None            =>
              val node = AlphaNode.Predicate(predicate, n.sources(predicate.sources.head))
              n.copy(topNodes = (tFacts -> node) +: n.topNodes)
          }
        }
      }
      add(facts, network)
    }
  }

  object MinimumBuffers extends AlphaNodeStrategy {
    override def addAlphaNode(
      network: AlphaNetwork.Intermediate,
      predicate: Predicate,
      facts: Set[Fact.Source[_]]
    ): AlphaNetwork.Intermediate = {
      val node = network
        .topNodes
        .collectFirst {
          case (f, n) if (facts -- f).isEmpty => n
        } match {
        case Some(value) => AlphaNode.Predicate(predicate, value)
        case None        => AlphaNode.Predicate(predicate, network.sources(predicate.sources.head))
      }
      network.copy(
        topNodes = (facts -> node) +: network.topNodes
      )
    }
  }
}
