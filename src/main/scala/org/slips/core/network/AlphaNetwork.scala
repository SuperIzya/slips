package org.slips.core.network

import org.slips.Env
import org.slips.core.build.PredicateMap
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate

case class AlphaNetwork(
  sources: Map[Condition.Source[_], AlphaNode] = Map.empty,
  topNodes: Map[Fact.Source[_], AlphaNode] = Map.empty
)

object AlphaNetwork {
  case class Intermediate(
    sources: Map[Condition.Source[_], AlphaNode] = Map.empty,
    topNodes: Map[Set[Fact.Source[_]], AlphaNode] = Map.empty
  ) {
    def toAlphaNetwork: AlphaNetwork = AlphaNetwork(
      sources = sources,
      topNodes = topNodes.flatMap { case (f, n) => f.map(_ -> n) }
    )
  }

  val empty: Intermediate = Intermediate()

  def apply(src: Set[Condition.Source[_]]): Intermediate =
    Intermediate(sources = src.map(s => s -> AlphaNode.Source(s)).toMap)

  extension (n: Intermediate)
    def add(p: Predicate, facts: Set[Fact.Source[_]]): Env[Intermediate] = env ?=> {
      val existing = n
        .topNodes
        .collectFirst {
          case (set, node) if (facts -- set).isEmpty => node
        }
      val node     = existing match {
        case Some(value) => AlphaNode.Predicate(p, value)
        case None        => AlphaNode.Predicate(p, n.sources(facts.head.sources.head))
      }
      n.copy(topNodes = n.topNodes + (facts -> node))
    }
}
