package org.slips.core.network

import cats.data.State
import org.slips.core.build.AlphaPredicate
import org.slips.core.fact.Fact

package object alpha {
  private[network] type FoldState[T] = State[FactsFolder, T]

  private[alpha] type PredicateToSignature = Map[AlphaPredicate, PredicateSignature]
  private[alpha] type SignatureToPredicate = Map[PredicateSignature, AlphaPredicate]
  private[alpha] type FactToSignature      = Map[Fact.Alpha[?], FactFullSignature]
  private[alpha] type FactToSuccessor      = Map[Fact.Alpha[?], Set[Fact.Alpha[?]]]
}
