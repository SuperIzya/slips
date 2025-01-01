package org.slips.core

import cats.data.State
import org.slips.core.build.BuildPredicate
import org.slips.core.fact.Fact
import org.slips.core.fact.Fact.Source

package object network {
  private[network] type FactProgress = FactProgress.InProgress | FactProgress.Done

  private[network] type FoldState[T] = State[FactsFolder, T]

  private[network] type PredicateToSignature = Map[BuildPredicate, PredicateSignature]
  private[network] type SignatureToPredicate = Map[PredicateSignature, BuildPredicate]
}
