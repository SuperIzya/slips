package org.slips.core.build

import cats.data.State
import cats.implicits.*
import cats.syntax.*
import org.slips.{Env, Environment, Signature}
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.network.{AlphaNode, NetworkLayer, Node}
import org.slips.core.rule.Rule

import scala.annotation.tailrec

private[build] case class BuildContext[F[_]](
  nodes: Map[String, Node] = Map.empty,
  nodeFacts: Map[Node, Set[Fact[?]]] = Map.empty,
  sources: Set[String] = Set.empty,
  sourceNodes: Map[String, AlphaNode.Source[?]] = Map.empty,
  allPredicates: AllPredicates = Map.empty,
  network: NetworkLayer = NetworkLayer.Empty,
  predicateRules: PredicateRules[F] = Map.empty[Predicate, Set[Rule[F]]],
  rules: Set[Rule[F]] = Set.empty[Rule[F]]
)

object BuildContext {

  def empty(using env: Environment): BuildContext[env.Effect] = BuildContext()

  extension [F[_]](ctx: BuildContext[F]) {
    def addNode(node: Node)(using env: Environment)(using ev: F =:= env.Effect): (BuildContext[env.Effect], Node) = {
      val signature = node.signature.compute
      val newCtx: BuildContext[env.Effect] = {
        if(ctx.nodes.contains(signature)) ev.liftCo(ctx)
        else ev.liftCo(ctx.copy(nodes = ctx.nodes + (signature -> node)))
      }
      newCtx -> node
    }

    def addSourceNode[T](
      signature: Signature,
      node: => AlphaNode.Source[T]
    )(using env: Environment)(using ev: F =:= env.Effect): (BuildContext[env.Effect], AlphaNode.Source[T]) = {
      val sign     = signature.compute
      val nextNode = ctx.sourceNodes.getOrElse(sign, node)
      ev.liftCo(ctx.copy(
        sources = ctx.sources + sign,
        sourceNodes = ctx.sourceNodes + (sign -> nextNode)
      )) -> nextNode.asInstanceOf[AlphaNode.Source[T]]
    }

    def addParsingResult(using env: Environment)(parseResult: ParseResult[env.Effect])(using ev: F =:= env.Effect): BuildContext[env.Effect] =
      ev.liftCo(ctx.copy(
        allPredicates = ctx.allPredicates |+| parseResult.allPredicates,
        sources = ctx.sources ++ parseResult.sources,
        rules = ctx.rules + parseResult.rule,
        predicateRules = ctx.predicateRules |+| parseResult.predicateRules
      ))

  }

  def addNetworkLayer(n: NetworkLayer): BuildStep[NetworkLayer] = BuildStep
    .update(ctx => ctx.copy(network = ctx.network.add(n)))
    .map(_ => n)
}
