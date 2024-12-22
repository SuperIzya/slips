package org.slips.core.network

import org.slips.core.fact.Fact

/**
  * Describes the progress of the facts collection while
  * building ReteNetwork
  */
private[network] object FactProgress {

  inline def fold[T](inline fp: FactProgress)
                    (inProgress: InProgress => T)
                    (done: Done => T): T = fp match {
    case ip: InProgress => inProgress(ip)
    case d: Done        => done(d)
  }

  extension (inline fp: FactProgress) {
    inline def fact: Fact.Source[?] = fold(fp)(_.fact)(_.fact)

    inline def topChain: Chain = fold(fp)(_.topChain)(_.topChain)
    
    inline def chains: Set[Chain] = fold(fp)(_.chains)(_.chains)
  }

  /** Fact not fully collected */
  final case class InProgress(
    fact: Fact.Source[?],
    chains: Set[Chain],

    /** Already united chains */
    united: Set[Chain],

    /** Chains left to unite */
    left: Set[Chain],

    /** Top chain uniting all chains from [[united]] */
    topChain: Chain
  ) {
    def done: Done = Done(fact, chains, topChain)
  }

  final case class Done(
    fact: Fact.Source[?],
    chains: Set[Chain],
    topChain: Chain
  )
}
