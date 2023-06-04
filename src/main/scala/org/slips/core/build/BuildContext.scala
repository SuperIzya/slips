package org.slips.core.build

import cats.data.State
import cats.implicits.*
import cats.syntax.*
import org.slips.Env
import org.slips.Signature
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.network.alpha.AlphaNetwork
import org.slips.core.network.alpha.AlphaNode
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM
import scala.annotation.tailrec

case class BuildContext private[build] (
  nodes: Map[String, Node] = Map.empty,
  nodeFacts: Map[Node, Set[Fact[_]]] = Map.empty,
  sources: Set[String] = Set.empty,
  sourceNodes: Map[String, AlphaNode.Source[_]] = Map.empty,
  predicateRules: PredicateRules = Map.empty,
  alphaPredicates: AlphaPredicates = Map.empty,
  betaPredicates: BetaPredicates = Map.empty,
  network: AlphaNetwork = AlphaNetwork.Empty,
  rules: Set[RuleM] = Set.empty
)

object BuildContext {

  val empty: BuildContext = BuildContext()

  extension (ctx: BuildContext) {
    def addNode(node: Node): (BuildContext, Node) = {
      val signature = node.signature
      ctx.copy(nodes = ctx.nodes + (signature -> node)) -> node
    }

    def addSource[T](source: Condition.Source[T]): Env[BuildContext] = env ?=> {
      val signature = env.signatureStrategy(source.signature)
      if (ctx.sources.contains(signature)) ctx
      else ctx.copy(sources = ctx.sources + signature)
    }

    def addSourceNode[T](
      signature: String,
      node: => AlphaNode.Source[T]
    ): (BuildContext, AlphaNode.Source[T]) = {
      val nextNode = ctx.sourceNodes.getOrElse(signature, node)
      ctx.copy(
        sources = ctx.sources + signature,
        sourceNodes = ctx.sourceNodes + (signature -> nextNode)
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
