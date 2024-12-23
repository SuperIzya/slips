package org.slips.core.build

import cats.implicits.*
import org.slips.Environment
import org.slips.Signature
import org.slips.core.build
import org.slips.core.fact.Fact
import org.slips.core.network.AlphaNode
import org.slips.core.network.NetworkLayer
import org.slips.core.network.Node
import org.slips.core.rule.Rule

private[build] final case class BuildContext[F[_]] private (
  nodes: Map[String, Node[F]],
  nodeFacts: Map[Node[F], Set[Fact[?]]],
  sources: Set[String],
  sourceNodes: Map[String, AlphaNode.Source[F, ?]],
  allPredicates: AllPredicates,
  network: NetworkLayer[F],
  predicateRules: PredicateRules[F],
  rules: Set[Rule[F]]
)

private[build] object BuildContext {

  def empty(using env: Environment): BuildContext[env.Effect] = BuildContext(
    nodes = Map.empty,
    nodeFacts = Map.empty,
    sources = Set.empty,
    sourceNodes = Map.empty,
    allPredicates = Map.empty,
    network = NetworkLayer.Empty(),
    predicateRules = Map.empty,
    rules = Set.empty
  )

  def addNetworkLayer(n: EnvNetworkLayer): EnvBuildStepF[NetworkLayer] =
    BuildStep.update(ctx => ctx.copy(network = ctx.network.add(n))).map(_ => n)

  extension [F[_]](ctx: BuildContext[F]) {

    def addNetwork(using env: Environment)(network: NetworkLayer[env.Effect])(using
      F: NetworkLayer[env.Effect] =:= NetworkLayer[F]): BuildStepF[F][NetworkLayer] = BuildStep.pureF {
      ctx.copy(network = ctx.network.add(F(network))) -> F(network)
    }

    def addNode(using env: Environment)(node: Node[env.Effect])(using
      F: Node[env.Effect] =:= Node[F]): BuildStepF[F][Node] = BuildStep.pureF {
      val signature = node.signature.compute
      val newCtx    = {
        if (ctx.nodes.contains(signature)) ctx
        else ctx.copy(nodes = ctx.nodes + (signature -> F(node)))
      }
      newCtx -> F(node)
    }

    def addSourceNode[T](using env: Environment)(signature: Signature, node: => AlphaNode.Source[env.Effect, T])(using
      F: AlphaNode.Source[env.Effect, T] =:= AlphaNode.Source[F, T]): BuildStepF[F][AlphaNode] =
      BuildStep.pureF {
        val sign                             = signature.compute
        val nextNode: AlphaNode.Source[F, T] = ctx
          .sourceNodes
          .get(sign)
          .collect { case a: AlphaNode.Source[F, T] => a }
          .getOrElse(F(node))

        val newCtx = ctx.copy(
          sources = ctx.sources + sign,
          sourceNodes = ctx.sourceNodes + (sign -> nextNode)
        )
        newCtx -> nextNode
      }

    def addParsingResult(using env: Environment)(parseResult: ParseResult[env.Effect])(using
      F: ParseResult[env.Effect] =:= ParseResult[F]): BuildContext[F] = {
      val res: ParseResult[F] = F(parseResult)
      ctx.copy(
        allPredicates = ctx.allPredicates |+| res.allPredicates,
        sources = ctx.sources ++ res.sources,
        rules = ctx.rules + res.rule,
        predicateRules = ctx.predicateRules |+| res.predicateRules
      )
    }
  }

}
