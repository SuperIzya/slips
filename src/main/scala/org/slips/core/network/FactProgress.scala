package org.slips.core.network

import org.slips.core.fact.Fact

/**
  * Describes the progress of the facts collection while
  * building AlphaNetwork
  */
sealed trait FactProgress {

  /** Source fact being tested */
  val fact: Fact.Source[?]

  /** All chains of predicates testing this source fact */
  val chains: Set[Chain]

  /** Top chain collecting all the [[chains]] */
  val topChain: Chain
}

object FactProgress {

  /** Fact not fully collected */
  case class InProgress(
    fact: Fact.Source[?],
    chains: Set[Chain],
    /** Already united chains */
    united: Set[Chain],
    /** Chains left to unite */
    left: Set[Chain],
    /** Top chain uniting all chains from [[united]] */
    topChain: Chain
  ) extends FactProgress

  case class Done(
    fact: Fact.Source[?],
    chains: Set[Chain],
    topChain: Chain
  ) extends FactProgress

}
