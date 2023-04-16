package org.slips.core.network.alpha

import org.slips.core.fact.Fact

private[alpha] case class PredicateSignature(predicate: String, ffs: Set[Fact.Alpha[_]])
