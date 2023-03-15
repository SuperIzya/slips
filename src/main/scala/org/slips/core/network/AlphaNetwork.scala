package org.slips.core.network

import cats.data.State
import cats.syntax.all.*
import org.slips.Env
import org.slips.core.build.AlphaPredicates
import org.slips.core.build.BuildStep
import org.slips.core.conditions
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.network
import org.slips.core.network.AlphaNode.Sources
import org.slips.core.network.alpha.*
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.collection.MapView
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

sealed trait AlphaNetwork {
  def sourceToNode: Map[Condition.Source[_], AlphaNode] = Map.empty
  val topNodes: Map[Fact.Source, AlphaNode]             = Map.empty

  def add(other: AlphaNetwork): AlphaNetwork
}

object AlphaNetwork {
  private val empty: Intermediate = Intermediate()

  /**
    * [[chains]] are sorted desc by the size of the facts
    * affected. Accumulated in [[Intermediate]] and
    * transformed to [[AlphaNetworkImpl]] in the end.
    *
    * @return
    */
  private def toNetwork(chains: Iterator[(Predicate.Alpha, Sources)]): BuildStep[AlphaNetwork] = {
    chains
      .flatMap { case (p, sources) => p.buildAlphaNode.map(_ -> sources) }
      .toList
      .sortBy(_._2.size)
      .foldLeft(BuildStep.pure(empty)) { case (itm, (pred, src)) =>
        itm.flatMap {
          _.add(pred, src)
        }
      }
      .map(_.toAlphaNetwork)
  }

  /**
    * Builds alpha network.
    *
    * Each alpha predicate may test several facts from the
    * same source but possibly from different rules.
    *
    * Each predicate that can be transformed to alpha node
    * is collected to [[AlphaNetwork]]
    */
  def apply(predicates: AlphaPredicates): BuildStep[AlphaNetwork] = {
    toNetwork(predicates.view.iterator)
  }

  case class Intermediate private[AlphaNetwork] (
    private val factsToChains: Map[Sources, Chain] = Map.empty,
    private val globalSources: Map[String, AlphaNode.Source[_]] = Map.empty
  ) {

    val addTupled: ((AlphaNode => AlphaNode, Sources)) => BuildStep[Intermediate] = (this.add _).tupled

    /**
      * Called from [[AlphaNetwork.Intermediate.addChain]]
      *
      * Accumulates alpha nodes for [[AlphaNetworkImpl]].
      * Accumulation is done according to following rules:
      *   1. predicate Q with same facts tested. If found,
      *      increase chain for these facts. Else next
      *   1. predicate S where S.facts contains P.facts. If
      *      found create chain for P.facts. Else next
      *   1. add predicate P to network as single chain for
      *      facts P.facts.
      */
    def add(predicate: AlphaNode => AlphaNode, sources: Sources): BuildStep[Intermediate] = {
      if (factsToChains.contains(sources)) {
        val tailChain = factsToChains(sources)
        val newChain  = Chain(predicate, tailChain, sources)
        BuildStep.pure(copy(factsToChains = factsToChains.updated(sources, newChain)))
      } else {
        factsToChains
          .values
          .find(c => sources.size < c.facts.size && sources.subsetOf(c.facts))
          .fold {
            val src = sources.head
            for {
              node <- globalSources.get(src.source.signature).fold(src.buildAlphaNode)(BuildStep.pure)
              chain = Chain(predicate, Chain(node, None, Set.empty), sources)
            } yield copy(
              globalSources = globalSources + (src.source.signature -> node),
              factsToChains = factsToChains + (sources              -> chain)
            )
          } { c =>
            val newChain = Chain(predicate, c, sources)
            BuildStep.pure(copy(factsToChains = factsToChains + (sources -> newChain)))
          }
      }
    }

    private def foldFacts: Map[Fact.Source, AlphaNode] = {

      val res: Map[Fact.Source, Set[Chain]] = factsToChains
        .view
        .iterator
        .flatMap { case (facts, chain) => facts.map(_ -> Set(chain)) }
        .toList
        .groupBy(_._1)
        .view
        .mapValues(_.flatMap(_._2).toSet)
        .toMap

      FactsFolder(res)
    }

    def toAlphaNetwork: AlphaNetwork = {
      val chains: Set[Chain] = factsToChains.values.toSet

      val sources: scala.collection.immutable.Map[String, AlphaNode.Source[_]] = chains
        .map(findSource)
        .map(_.signature)
        .flatMap(s => globalSources.get(s).map(s -> _))
        .toMap

      new AlphaNetworkImpl(
        sources = factsToChains.keySet.flatten.map { src => AlphaSource(src.source, src.source.extractNode(sources)) },
        topNodes = foldFacts,
        alphaNetwork = chains
      )
    }

    @tailrec
    private def findSource(chain: Chain): AlphaNode.Source[_] = {
      chain.tail.fold(chain.head.sourceNode)(findSource)
    }
  }

  private[AlphaNetwork] sealed trait AlphaSource {
    type Type
    val condition: Condition.Source[Type]
    val node: AlphaNode.Source[Type]
  }
  private[AlphaNetwork] object AlphaSource       {
    def apply[T](condSource: Condition.Source[T], nodeSource: AlphaNode.Source[T]): AlphaSource = new AlphaSource {
      override type Type = T
      override val node: AlphaNode.Source[T]      = nodeSource
      override val condition: Condition.Source[T] = condSource
    }
  }

  private class AlphaNetworkImpl(
    private[AlphaNetwork] val sources: Set[AlphaSource],
    override val topNodes: Map[Fact.Source, AlphaNode],
    private[AlphaNetwork] val alphaNetwork: Set[Chain]
  ) extends AlphaNetwork {
    override def add(other: AlphaNetwork): AlphaNetwork = other match
      case impl: AlphaNetworkImpl =>
        new AlphaNetworkImpl(
          sources = this.sources ++ impl.sources,
          topNodes = topNodes ++ impl.topNodes,
          alphaNetwork = alphaNetwork ++ impl.alphaNetwork
        )
      case Empty                  => this
  }

  case object Empty extends AlphaNetwork {
    override def add(other: AlphaNetwork): AlphaNetwork = other
  }
}
