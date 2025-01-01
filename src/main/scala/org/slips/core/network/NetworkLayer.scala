package org.slips.core.network

import cats.syntax.all.*
import org.slips.Env
import org.slips.core.build.*
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.network
import org.slips.core.network.*
import scala.annotation.targetName
import scala.collection.immutable.SortedMap

sealed trait NetworkLayer[F[_]] {
  // Source signature -> Source alpha node -- Data will be passed there
  // TODO: Is this needed?
  val inlets: Map[String, AlphaNode.Source[F, ?]] = Map.empty
  // Fact signature ->
  val outlets: Map[String, AlphaNode[F]]          = Map.empty

  val topChains: Map[Fact.Source[?], Chain]
  private[core] val networkLayer: Set[Chain]
  def add(other: NetworkLayer[F]): NetworkLayer[F]
}

object NetworkLayer {

  /**
    * Builds the network layer.
    *
    * Each layer has an increasing arity of [[Fact.Val]].
    * For the first layer the following rules apply:
    *
    * Each alpha predicate may test several facts from the
    * same source but possibly from different rules.
    *
    * [[predicates]] are sorted desc by the size of the
    * facts affected. Accumulated in [[Intermediate]] and
    * transformed to [[Impl]] in the end.
    *
    * For predicates P1 ... P7 and facts F1 ... F6 where
    * facts are tested by predicates as following:
    * {{{
    * - F1 - P1, P2, P6, P7
    * - F2 - P2, P3, P4, P6, P7
    * - F3 - P3, P4, P5, P6, P7
    * - F4 - P1, P3, P4, P6, P7
    * - F5 - P1, P2, P3, P4, P6, P7
    * - F6 - P1, P2, P3, P5, P6, P7
    * }}}
    *
    * Then predicates are testing facts as following:
    * {{{
    *  -P1 - F1, F4, F5, F6
    *  -P2 - F1, F2, F5, F6
    *  -P3 - F2, F3, F4, F5, F6
    *  -P4 - F2, F3, F4, F5
    *  -P5 - F3, F6
    *  -P6 - F1, F2, F3, F4, F5, F6
    *  -P7 - F1, F2, F3, F4, F5, F6
    * }}}
    *
    * The predicates will be added to the network from less
    * facts to more facts, while the collected predicates
    * will be added in reverse order: from most facts to
    * less
    *
    * Step 1:
    * {{{
    * -P5 - F3, F6
    * }}}
    * Step 2-4:
    * {{{
    *  -P4 - F2, F3, F4, F5
    *  -P2 - F1, F2, F5, F6
    *  -P1 - F1, F4, F5, F6
    *  -P5 - F3, F6
    * }}}
    * There is no pair from these predicates that their sets
    * of facts are subset of one another.
    *
    * Step 5 (adding P3):
    * {{{
    *  -P3 - F2, F3, F4, F5, F6 ----> P4 (F2, F3, F4, F5)
    *  -P4 ----------------------------||
    *  -P2 - F1, F2, F5, F6
    *  -P1 - F1, F4, F5, F6
    *  -P5 - F3, F6
    * }}}
    * Each predicate from step 4 can be thought of as a
    * chain of one. The chains are either extended or
    * updated depending whether facts for predicate in
    * question (P3) are a subset of one of the already added
    * chains or exactly the same.
    *
    * Step 6, 7:
    * {{{
    *  -P6,P7 - F1, F2, F3, F4, F5, F6 ----> P3 (F2, F3, F4, F5, F6) ----> P4 (F2, F3, F4, F5)
    *  -P3 ----------------------------------||                            ||
    *  -P4 ----------------------------------------------------------------||
    *  -P2 - F1, F2, F5, F6
    *  -P1 - F1, F4, F5, F6
    *  -P5 - F3, F6
    * }}}
    *
    * At step 7 P7 was added alongside P6 since their facts
    * are exactly the same.
    *
    * Now that the network of predicates is built facts
    * needs to be collected into single nodes to provide
    * that further to next network layer.
    *
    * First, all chains need to be reversed: P6 alone is
    * good only for fact F1. All the rest of the facts
    * should be collected from the tail.
    *
    * {{{
    * -P6,P7 - F1 <---- P3 (F6) <---- P4 (F2, F3, F4, F5)
    * -P3 --------------||            ||
    * -P4 ----------------------------||
    * -P2 - F1, F2, F5, F6
    * -P1 - F1, F4, F5, F6
    * -P5 - F3, F6
    * }}}
    * Fact F1 (as the one with most predicates) needs
    * predicates P6, P7, P2 & P1. At the beginning there are
    * no union nodes, so it make sense to unite P2 & P1,
    * since they both have most facts.
    *
    * Node `P1 & P2 (F1, F5, F6)` acts as a beta node but
    * actually is an alpha node. P6, P7 are bound together,
    * they will be represented as chain of nodes, but for
    * now for all intends and purposes they are one node.
    * Another union node `P6,P7 & (P1 & P2)` and fact F1 is
    * ready for consumption by next network layer.
    *
    * For fact F2 it's P4 from the chain and P2 (since chain
    * starting with P4 already contains P3, P6, P7). So
    * another union node appears `P4 & P2 (F2, F5)`. etc.
    */
  def apply(predicates: AllPredicates): EnvNetworkLayer = env ?=> {
    // Range all predicates by arity
    val predicatesByArity = predicates.values.map(p => p.arity -> p).groupBy(_._1)

    val signedPredicates: PredicateToSignature = predicates
      .values
      .map { ap =>
        ap -> PredicateSignature(
          ap.predicate.signature.compute,
          ap.facts
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
      .sorted(using Ordering.fromLessThan[BuildPredicate]((a, b) => a.facts.size < b.facts.size))
      .foldLeft(Intermediate(signedPredicates, predicatesBySign))(_ |+| _)
      .toNetworkLayer
  }

  private[NetworkLayer] given Ordering[Set[Fact.Source[?]]] = Ordering.fromLessThan((x, y) => x.size > y.size)
  private[NetworkLayer] case class Intermediate(
    signedPredicates: PredicateToSignature,
    predicatesBySign: SignatureToPredicate,
    private val factsToChains: SortedMap[Set[Fact.Source[?]], Chain.Predicates] = SortedMap.from(Map.empty)
  ) {

    /**
      * Accumulates alpha nodes for [[Impl]]. Accumulation
      * is done according to following rules:
      *   1. predicate Q with same facts tested. If found,
      *      increase chain for these facts. Else next
      *   1. predicate S where S.facts contains P.facts. If
      *      found create chain for P.facts. Else next
      *   1. add predicate P to network as single chain for
      *      facts P.facts.
      */
    @targetName("flus")
    infix def |+|(predicate: BuildPredicate): Env[Intermediate] = {
      val facts = predicate.facts

      if (factsToChains.contains(facts)) {
        val chain    = factsToChains(facts)
        val newChain = chain.appendPredicate(predicate)

        copy(factsToChains = factsToChains.updated(facts, newChain))
      } else {
        factsToChains
          .find(_._1.subsetOf(facts))
          .fold {
            copy(factsToChains = factsToChains + (facts -> Chain(predicate, facts)))
          } { c =>
            val newChain = Chain(predicate, facts, c._2)
            copy(factsToChains = factsToChains + (facts -> newChain))
          }
      }
    }

    private def foldFacts: Env[Map[Fact.Source[?], Chain]] = {

      val res: Map[Fact.Source[?], Set[Chain]] = factsToChains
        .view
        .flatMap { case (_, chain) =>
          val inverted = chain.invert
          inverted.facts.map(_ -> inverted)
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).toSet)
        .toMap

      FactsFolder(res)
    }

    val toNetworkLayer: EnvNetworkLayer = env ?=> {
      val folded: Map[Fact.Source[?], Chain] = foldFacts
      val chains: Set[Chain]                 = folded.values.toSet

      new Impl(
        topChains = folded,
        networkLayer = chains
      )
    }
  }

  private class Impl[F[_]](
    val topChains: Map[Fact.Source[?], Chain],
    private[core] val networkLayer: Set[Chain]
  ) extends NetworkLayer[F] {
    override def add(other: NetworkLayer[F]): NetworkLayer[F] = other match {
      case impl: Impl[F] =>
        new Impl(
          topChains = topChains ++ impl.topChains,
          networkLayer = networkLayer ++ impl.networkLayer
        )
      case _: Empty[F]   => this
    }
  }

  case class Empty[F[_]]() extends NetworkLayer[F] {
    override private[core] val networkLayer                   = Set.empty[Chain]
    override val topChains: Map[Fact.Source[?], Chain]        = Map.empty
    override def add(other: NetworkLayer[F]): NetworkLayer[F] = other
  }
}
