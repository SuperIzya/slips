package org.slips.core.network.alpha

import cats.data.State
import cats.syntax.all.*
import org.slips.Env
import org.slips.core.build
import org.slips.core.build.*
import org.slips.core.conditions
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.network
import org.slips.core.network.alpha.*
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.collection.MapView
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

sealed trait AlphaNetwork {
  // Source signature -> Source alpha node -- Data will be passed there
  // TODO: Is this needed?
  val inlets: Map[String, AlphaNode.Source[_]] = Map.empty
  // Fact signature ->
  val outlets: Map[String, AlphaNode]          = Map.empty

  def add(other: AlphaNetwork): AlphaNetwork
}

object AlphaNetwork {
  private val empty: Intermediate = Intermediate()

  private case class FactFullSignature(fact: String, predicates: Set[String]) {
    def add(p: String): FactFullSignature = copy(predicates = predicates + p)
  }

  // Full signature of the predicate
  private case class PredicateSignature(predicate: String, ffs: Option[FactFullSignature])

  /**
    * Builds alpha network.
    *
    * Each alpha predicate may test several facts from the
    * same source but possibly from different rules.
    *
    * [[predicates]] are sorted desc by the size of the
    * facts affected. Accumulated in [[Intermediate]] and
    * transformed to [[AlphaNetworkImpl]] in the end.
    */
  def apply(predicates: AlphaPredicates): AlphaNetwork = {
    // Collect all predicate's signatures applied to a fact and it's predecessors
    given ord: Ordering[(Fact.Alpha[_], String)] = Ordering
      .fromLessThan((a, b) => a._1.predecessors.size < b._1.predecessors.size)
    val (factsSigns: Map[Fact.Alpha[_], FactFullSignature], successors: Map[Fact.Alpha[_], Set[Fact.Alpha[_]]]) =
      predicates
        .flatMap(ap => ap.facts.map(_ -> ap.predicate.signature))
        .toList
        .sorted
        .foldLeft((Map.empty[Fact.Alpha[_], FactFullSignature], Map.empty[Fact.Alpha[_], Set[Fact.Alpha[_]]])) {
          case ((signs, succ), f -> p) =>
            signs.updated(f, signs.getOrElse(f, FactFullSignature(f.signature, Set.empty)).add(p)) ->
              f.predecessors.foldLeft(succ)((s, p) => s + (p -> (s.getOrElse(p, Set.empty) + f)))
        }

    type PerPredicate[T] = Map[AlphaPredicate, T]
    val signedPredicates: PerPredicate[PredicateSignature] = predicates
      .map { ap =>
        val fact = ap.facts.head
        val sign = PredicateSignature(ap.predicate.signature, factsSigns.get(fact))
        ap -> sign
      }
      .toMap

    val predicatesBySign: Map[PredicateSignature, AlphaPredicate] = signedPredicates
      .view
      .map(_.swap)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).reduceLeft(_ |+| _))
      .toMap

    predicatesBySign
      .view
      .values
      .toList
      .sorted(using Ordering.fromLessThan[AlphaPredicate]((a, b) => a.facts.size < b.facts.size))
      .foldLeft(empty) { _ add _ }
      .toAlphaNetwork
  }

  private given Ordering[Set[Fact.Alpha[_]]] = Ordering.fromLessThan((a, b) => a.size < b.size)

  private[AlphaNetwork] case class Intermediate(
    private val factsToChains: SortedMap[FactFullSignature, Chain] = SortedMap.from(Map.empty),
    private val globalSources: Map[String, AlphaPredicate] = Map.empty
  ) {

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
    def add(predicate: AlphaPredicate): Intermediate = {
      val facts = predicate.facts
      if (factsToChains.contains(facts)) {
        val tailChain = factsToChains(facts)
        val newChain  = Chain(predicate, tailChain)
        copy(factsToChains = factsToChains.updated(facts, newChain))
      } else {
        factsToChains
          .values
          .dropWhile(_.facts.size < facts.size)
          .find(c => facts.subsetOf(c.facts))
          .fold {
            val src   = facts.head.sourceFact.source
            val node  = globalSources.getOrElse(src.signature, AlphaPredicate(predicate.source))
            val chain = Chain(predicate, Chain(node, None, Set.empty))
            copy(
              globalSources = globalSources + (src.signature -> node),
              factsToChains = factsToChains + (sources       -> chain)
            )
          } { c =>
            val newChain = Chain(predicate, c, facts)
            copy(factsToChains = factsToChains + (facts -> newChain))
          }
      }
    }

    private def foldFacts: Map[FactFullSignature, AlphaNode] = {

      val res: Map[FactFullSignature, Set[Chain]] = factsToChains
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
