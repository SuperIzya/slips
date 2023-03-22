package org.slips.core.network

import org.slips.core.fact.Fact

case class FactDescriptor(
  signature: String,      // Full signature of the fact - all transformations from sources till now
  facts: Set[Fact[_]],    // Instances of facts that fit that description
  predicates: Set[String] // Signatures of all predicates applied for this signature
)
