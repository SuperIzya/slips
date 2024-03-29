package org.slips.core.build

import cats.data.State
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.network.AlphaNetwork
import org.slips.core.network.Node
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM
import scala.annotation.tailrec

case class BuildContext private[build] (
  nodes: Map[String, Node] = Map.empty,
  sources: Set[Condition.Source[_]] = Set.empty,
  alphaPredicates: PredicateMap = Map.empty,
  betaPredicates: PredicateMap = Map.empty,
  gammaPredicates: PredicateMap = Map.empty,
  network: AlphaNetwork = AlphaNetwork(),
  rules: Set[RuleM] = Set.empty
)

object BuildContext {

  val empty: BuildContext = BuildContext()

  private def combine(
    a: PredicateMap,
    b: PredicateMap
  ): PredicateMap = {
    @tailrec
    def doCombine(
      x: PredicateMap,
      y: PredicateMap,
      res: PredicateMap
    ): PredicateMap = {
      if (x.isEmpty) res ++ y
      else if (y.isEmpty) res ++ x
      else {
        val (headP, headF) = x.head
        doCombine(x - headP, y - headP, res + (headP -> (headF ++ y.getOrElse(headP, Set.empty))))
      }
    }
    doCombine(a, b, Map.empty)
  }

  extension (
    ctx: BuildContext
  ) {
    def addNode(
      node: Node
    ): (
      BuildContext,
      Node
    ) =
      ctx.copy(nodes = ctx.nodes + (node.signature -> node)) -> node

    def addSource[T](
      source: Condition.Source[T]
    ): BuildContext = {
      if (ctx.sources.contains(source)) ctx
      else ctx.copy(sources = ctx.sources + source)
    }

    def addParsingResult(
      parseResult: ParseResult
    ): BuildContext =
      ctx.copy(
        alphaPredicates = combine(ctx.alphaPredicates, parseResult.alphaPredicates),
        betaPredicates = combine(ctx.betaPredicates, parseResult.betaPredicates),
        gammaPredicates = combine(ctx.gammaPredicates, parseResult.gammaPredicates),
        sources = ctx.sources ++ parseResult.sources,
        rules = ctx.rules + parseResult.rule
      )

    def addAlphaNetwork(
      n: AlphaNetwork
    ): BuildContext = ctx.copy(network = n)
  }
}
