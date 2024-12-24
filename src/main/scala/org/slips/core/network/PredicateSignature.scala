package org.slips.core.network

import org.slips.core.fact.Fact

private[network] case class PredicateSignature(predicate: String, ffs: Set[Fact.Source[?]])
