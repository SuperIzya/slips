package org.slips.core.build

import cats.data.State
import cats.implicits.*
import cats.syntax.*
import org.slips.Env
import org.slips.Environment
import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.network.alpha.AlphaNetwork
import org.slips.core.network.alpha.AlphaNode
import org.slips.core.rule.Rule.RuleM
import scala.annotation.tailrec

case class BuildContext private[build] (
  nodes: Map[String, Node] = Map.empty,
  nodeFacts: Map[Node, Set[Fact[?]]] = Map.empty,
  sources: Set[String] = Set.empty,
  sourceNodes: Map[String, AlphaNode.Source[?]] = Map.empty,
  predicateRules: PredicateRules = Map.empty,
  alphaPredicates: AlphaPredicates = Map.empty,
  betaPredicates: BetaPredicates = Map.empty,
  network: AlphaNetwork = AlphaNetwork.Empty,
  rules: Set[RuleM] = Set.empty
)

object BuildContext {

  val empty: BuildContext = BuildContext()

  extension (ctx: BuildContext) {
    def addNode(node: Node)(using env: Environment): (BuildContext, Node) = {
      val signature = node.signature
      ctx.copy(nodes = ctx.nodes + (signature.compute -> node)) -> node
    }

    def addSourceNode[T](
      signature: Signature,
      node: => AlphaNode.Source[T]
    )(using env: Environment): (BuildContext, AlphaNode.Source[T]) = {
      val sign     = signature.compute
      val nextNode = ctx.sourceNodes.getOrElse(sign, node)
      ctx.copy(
        sources = ctx.sources + sign,
        sourceNodes = ctx.sourceNodes + (sign -> nextNode)
      ) -> nextNode.asInstanceOf[AlphaNode.Source[T]]
    }

    def addParsingResult(parseResult: ParseResult): BuildContext =
      ctx.copy(
        alphaPredicates = ctx.alphaPredicates |+| parseResult.alphaPredicates,
        betaPredicates = ctx.betaPredicates |+| parseResult.betaPredicates,
        sources = ctx.sources ++ parseResult.sources,
        rules = ctx.rules + parseResult.rule,
        predicateRules = ctx.predicateRules |+| parseResult.predicateRules
      )

  }

  def addAlphaNetwork(n: AlphaNetwork): BuildStep[AlphaNetwork] = BuildStep
    .update(ctx => ctx.copy(network = ctx.network.add(n)))
    .map(_ => n)
}
