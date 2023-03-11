package org.slips.core.build.strategy

import cats.Monoid
import cats.data.NonEmptySet
import org.slips.core.build.BuildContext
import org.slips.core.build.BuildStep
import org.slips.core.build.PredicateRules
import org.slips.core.fact.Fact
import org.slips.core.network.AlphaNetwork
import org.slips.core.network.AlphaNode
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.math.Ordered.orderingToOrdered

/** Strategy to place alpha-nodes */
trait AlphaNodeStrategy {

  private[build] def buildStep: BuildStep[AlphaNetwork] = BuildStep { ctx =>
    val network = buildNetwork(ctx)
    ctx.addAlphaNetwork(network) -> network
  }

  protected def buildNetwork(ctx: BuildContext): AlphaNetwork = {
    ctx.alphaPredicates.toList.sortBy(-_._2.size).foldLeft(AlphaNetwork(ctx.sources))(fold).toAlphaNetwork
  }

  def fold(network: AlphaNetwork.Intermediate, pair: (Predicate, Set[Fact.Source[_]])): AlphaNetwork.Intermediate =
    addAlphaNode(network, pair._1, pair._2)

  def addAlphaNode(
    network: AlphaNetwork.Intermediate,
    predicate: Predicate,
    facts: Set[Fact.Source[_]]
  ): AlphaNetwork.Intermediate
}

object AlphaNodeStrategy {

  object OptimizeLoad extends AlphaNodeStrategy {

    opaque type Estimation = Double

    object Estimation {
      def apply(count: Int, rules: Double): Estimation    = 1.0 / (rules * count)
      def apply(count: Double, rules: Double): Estimation = 1.0 / (rules * count)
    }

    case class Unordered(
      predicates: SortedMap[Predicate, Set[RuleM]],
      rulesPCount: Map[RuleM, Double],
      rulesPredicate: Map[RuleM, Set[Predicate]],
      predicateRules: PredicateRules
    ) {
      def remove(p: Iterable[Predicate]): Unordered = copy(predicates = predicates.removedAll(p))
    }

    object Unordered {
      def apply(predicateRules: PredicateRules): Unordered = {
        val rulesPredicateView = predicateRules
          .toList
          .flatMap { case (k, v) => v.map(_ -> k) }
          .groupBy(_._1)
          .view
          .mapValues(_.map(_._2).toSet)
        Unordered(
          predicateRules.toList.sortBy(-_._2.size).to[SortedMap],
          rulesPredicateView.mapValues(g => 1.0 / g.size).toMap,
          rulesPredicateView.toMap,
          predicateRules
        )
      }

    }

    case class Cluster(predicates: NonEmptySet[Predicate], rules: Set[RuleM], estimation: Estimation)
    object Cluster {
      def apply(predicate: Predicate, unordered: Unordered): Cluster =
        apply(NonEmptySet.of(predicate), unordered)

      def apply(predicates: NonEmptySet[Predicate], unordered: Unordered): Cluster = {
        val rules: Set[RuleM] = predicates.flatMap(p => unordered.predicateRules(p))
        Cluster(
          predicates = predicates,
          rules = rules,
          estimation = Estimation(
            predicates.size,
            rules.map(unordered.rulesPCount(_)).sum
          )
        )
      }

      given Ordering[Cluster] = Ordering.fromLessThan[Cluster](_.estimation > _.estimation)
    }

    case class PartialSolution(
      clusters: List[Cluster],
      unordered: Unordered,
      rulesPCount: Map[RuleM, Double],
      estimation: Estimation
    ) {
      def addCluster(cluster: Cluster, xsters: Cluster*): PartialSolution = {
        val clsX        = xsters :+ cluster
        val newClusters = (clusters ++ clsX).sorted
        val newPCount   = clsX
          .flatMap(_.rules)
          .foldLeft(rulesPCount) { (cnt, r) => cnt + (r -> (cnt.getOrElse(r, 0.0) + unordered.rulesPCount(r))) }
        PartialSolution(
          newClusters,
          unordered.remove(clsX.flatMap(_.predicates)),
          newPCount,
          newClusters.map(_.estimation).sum
        )

      }
    }
    override def addAlphaNode(
      network: AlphaNetwork.Intermediate,
      predicate: Predicate,
      facts: Set[Fact.Source[_]]
    ): AlphaNetwork.Intermediate = ???

    override protected def buildNetwork(ctx: BuildContext): AlphaNetwork = {}
  }

  /**
    * Maximizing the chains of predicate for facts, possibly
    * having the same alpha node several times in the
    * network
    */
  object MaximizeChains extends AlphaNodeStrategy {
    override def addAlphaNode(
      network: AlphaNetwork.Intermediate,
      predicate: Predicate,
      facts: Set[Fact.Source[_]]
    ): AlphaNetwork.Intermediate = {
      @tailrec
      def add(tFacts: Set[Fact.Source[_]], n: AlphaNetwork.Intermediate): AlphaNetwork.Intermediate = {
        if (tFacts.isEmpty) n
        else {
          network.topNodes.map { case (f, n) => tFacts.intersect(f) -> n }.maxByOption(_._1.size) match {
            case Some((f, prev)) =>
              val node = AlphaNode.Predicate(predicate, prev)
              add(tFacts -- f, n.copy(topNodes = n.topNodes + (f -> node)))
            case None            =>
              val node = AlphaNode.Predicate(predicate, n.sources(predicate.sources.head))
              n.copy(topNodes = n.topNodes + (tFacts -> node))
          }
        }
      }
      add(facts, network)
    }
  }

  /**
    * Allocate alpha node only once, thus reducing number of
    * buffers.
    */
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
        topNodes = network.topNodes + (facts -> node)
      )
    }
  }
}
