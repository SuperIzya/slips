package org.slips.core

import org.slips.core.fact.Fact

package object network {
  type Source[T] = T match
    case x <:< Tuple => Tuple.Map[x, Source]
    case _           => Node

  // Signature of the source of facts for beta network.
  // Source signature -> Set of signatures of alpha predicates
  type FactSourceSignature = (String, Set[String])

  type Sources = Set[Fact.Source]
}
