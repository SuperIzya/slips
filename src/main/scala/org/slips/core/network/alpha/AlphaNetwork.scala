package org.slips.core.network.alpha

import cats.Applicative
import cats.Semigroup
import cats.data.State
import cats.kernel.Monoid
import cats.syntax.all.*
import org.slips.Env
import org.slips.core.build
import org.slips.core.build.*
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.network
import org.slips.core.network.alpha.*
import scala.annotation.showAsInfix
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.collection.MapView
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

sealed trait AlphaNetwork {
  // Source signature -> Source alpha node -- Data will be passed there
  // TODO: Is this needed?
  val inlets: Map[String, AlphaNode.Source[?]] = Map.empty
  // Fact signature ->
  val outlets: Map[String, AlphaNode]          = Map.empty

  val topChains: Map[Fact.Alpha[?], Chain]
  private[core] val alphaNetwork: Set[Chain]
  def add(other: AlphaNetwork): AlphaNetwork
}

object AlphaNetwork {

  /**
    * Builds alpha network.
    *
    * Each alpha predicate may test several facts from the
    * same source but possibly from different rules.
    *
    * [[predicates]] are sorted desc by the size of the
    * facts affected. Accumulated in [[Intermediate]] and
    * transformed to [[AlphaNetworkImpl]] in the end.
    *
    * For predicates P1 ... P7 and facts F1 ... F6 where
    * facts are tested by predicates as following:
    * ```
    * -F1 - P1, P2, P6, P7
    * -F2 - P2, P3, P4, P6, P7
    * -F3 - P3, P4, P5, P6, P7
    * -F4 - P1, P3, P4, P6, P7
    * -F5 - P1, P2, P3, P4, P6, P7
    * -F6 - P1, P2, P3, P5, P6, P7
    * ```
    *
    * Then predicates are testing facts as following:
    * ```
    *  -P1 - F1, F4, F5, F6
    *  -P2 - F1, F2, F5, F6
    *  -P3 - F2, F3, F4, F5, F6
    *  -P4 - F2, F3, F4, F5
    *  -P5 - F3, F6
    *  -P6 - F1, F2, F3, F4, F5, F6
    *  -P7 - F1, F2, F3, F4, F5, F6
    * ```
    *
    * The predicates will be added to the network from less
    * facts to more facts, while the collected predicates
    * will be added in reverse order: from most facts to
    * less
    *
    * Step 1:
    *
    * -P5 - F3, F6
    *
    * Step 2-4:
    * ```
    *  -P4 - F2, F3, F4, F5
    *  -P2 - F1, F2, F5, F6
    *  -P1 - F1, F4, F5, F6
    *  -P5 - F3, F6
    * ```
    * There is no pair from these predicates that their sets
    * of facts are subset of one another.
    *
    * Step 5 (adding P3):
    * ```
    *  -P3 - F2, F3, F4, F5, F6 ----> P4 (F2, F3, F4, F5)
    *  -P4 ----------------------------||
    *  -P2 - F1, F2, F5, F6
    *  -P1 - F1, F4, F5, F6
    *  -P5 - F3, F6
    * ```
    * Each predicate from step 4 can be thought of as a
    * chain of one. The chains are either extended or
    * updated depending whether facts for predicate in
    * question (P3) are a subset of one of the already added
    * chains or exactly the same.
    *
    * Step 6, 7:
    * ```
    *  -P6,P7 - F1, F2, F3, F4, F5, F6 ----> P3 (F2, F3, F4, F5, F6) ----> P4 (F2, F3, F4, F5)
    *  -P3 ----------------------------------||                            ||
    *  -P4 ----------------------------------------------------------------||
    *  -P2 - F1, F2, F5, F6
    *  -P1 - F1, F4, F5, F6
    *  -P5 - F3, F6
    * ```
    *
    * At step 7 P7 was added alongside P6 since their facts
    * are exactly the same.
    *
    * Now that the network of predicates is built facts
    * needs to be collected into single nodes to provide
    * that further to beta network.
    *
    * First, all chains need to be reversed: P6 alone is
    * good only for fact F1. All the rest of the facts
    * should be collected from the tail somewhere.
    *
    * ```
    * -P6,P7 - F1 <---- P3 (F6) <---- P4 (F2, F3, F4, F5)
    * -P3 --------------||            ||
    * -P4 ----------------------------||
    * -P2 - F1, F2, F5, F6
    * -P1 - F1, F4, F5, F6
    * -P5 - F3, F6
    * ```
    * Fact F1 (as the one with fewer predicates) needs
    * predicates P6, P7, P2 & P1. At the beginning there are
    * no union nodes, so it make sense to unite P2 & P1,
    * since they both have most facts.
    *
    * Node `P1 & P2 (F1, F5, F6)` acts as a beta node but
    * actually is an alpha node. P6, P7 are bound together,
    * they will be represented as chain of nodes, but for
    * for now for all intends and purposes they are one
    * node. Another union node `P6,P7 & (P1 & P2)` and fact
    * F1 is ready for consumption by beta network.
    *
    * For fact F2 it's P4 from the chain and P2 (since chain
    * starting with P4 already contains P3, P6, P7). So
    * another union node appears `P4 & P2 (F2, F5)`. etc.
    */
  def apply(predicates: AlphaPredicates): Env[AlphaNetwork] = env ?=> {
    // Collect all predicate's signatures applied to a fact and it's predecessors
    val successors: FactToSuccessor =
      predicates
        .values
        .flatMap(ap => ap.facts.map(_ -> ap.predicate.signature))
        .foldLeft[FactToSuccessor](Map.empty) { case (succ, f -> _) =>
          f.predecessors.foldLeft(succ)((s, p) => s + (p -> (s.getOrElse(p, Set.empty) + f)))
        }

    val signedPredicates: PredicateToSignature = predicates
      .values
      .map { ap =>
        ap -> PredicateSignature(
          ap.predicate.signature.compute,
          ap.facts.flatMap(f => successors.get(f).toSet.flatten + f)
        )
      }
      .toMap

    val predicatesBySign: SignatureToPredicate = signedPredicates
      .view
      .map(_.swap)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).reduceLeft(_ |+| _))
      .toMap

    predicatesBySign
      .values
      .toList
      .sorted(using Ordering.fromLessThan[AlphaPredicate]((a, b) => a.facts.size < b.facts.size))
      .foldLeft(Intermediate(successors, signedPredicates, predicatesBySign))(_ |+| _)
      .toAlphaNetwork
  }

  private[AlphaNetwork] given Ordering[Set[Fact.Alpha[?]]] = Ordering.fromLessThan((x, y) => x.size > y.size)
  private[AlphaNetwork] case class Intermediate(
    successors: FactToSuccessor,
    signedPredicates: PredicateToSignature,
    predicatesBySign: SignatureToPredicate,
    private val factsToChains: SortedMap[Set[Fact.Alpha[?]], Chain.Predicates] = SortedMap.from(Map.empty)
  ) {

    /**
      * Accumulates alpha nodes for [[AlphaNetworkImpl]].
      * Accumulation is done according to following rules:
      *   1. predicate Q with same facts tested. If found,
      *      increase chain for these facts. Else next
      *   1. predicate S where S.facts contains P.facts. If
      *      found create chain for P.facts. Else next
      *   1. add predicate P to network as single chain for
      *      facts P.facts.
      */
    @showAsInfix @targetName("flus")
    def |+|(predicate: AlphaPredicate): Env[Intermediate] = {
      val facts = predicate.facts

      if (factsToChains.contains(facts)) {
        val chain    = factsToChains(facts)
        val newChain = chain.appendPredicate(predicate, facts)

        copy(factsToChains = factsToChains + (facts -> newChain))
      } else {
        factsToChains
          .values
          .toList
          .find(c => c.facts.subsetOf(facts))
          .fold {
            copy(factsToChains = factsToChains + (facts -> Chain(predicate, facts)))
          } { c =>
            val newChain = Chain(predicate, facts, c)
            copy(factsToChains = factsToChains + (facts -> newChain))
          }
      }
    }

    private def foldFacts: Env[Map[Fact.Alpha[?], Chain]] = {

      val res: Map[Fact.Alpha[?], Set[Chain]] = factsToChains
        .view
        .iterator
        .flatMap { case (_, chain) =>
          val inverted = chain.invert(None)
          inverted.facts.map(_ -> inverted)
        }
        .toList
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).toSet)
        .toMap

      FactsFolder(res)
    }

    def toAlphaNetwork: Env[AlphaNetwork] = {
      val folded: Map[Fact.Alpha[?], Chain] = foldFacts
      val chains: Set[Chain]                = folded.values.toSet

      new AlphaNetworkImpl(
        topChains = folded,
        alphaNetwork = chains
      )
    }

  }

  private class AlphaNetworkImpl(
    val topChains: Map[Fact.Alpha[?], Chain],
    private[core] val alphaNetwork: Set[Chain]
  ) extends AlphaNetwork {
    override def add(other: AlphaNetwork): AlphaNetwork = other match
      case impl: AlphaNetworkImpl =>
        new AlphaNetworkImpl(
          topChains = topChains ++ impl.topChains,
          alphaNetwork = alphaNetwork ++ impl.alphaNetwork
        )
      case Empty                  => this
  }

  case object Empty extends AlphaNetwork {
    override private[core] val alphaNetwork             = Set.empty[Chain]
    override val topChains: Map[Fact.Alpha[?], Chain]   = Map.empty
    override def add(other: AlphaNetwork): AlphaNetwork = other
  }
}
