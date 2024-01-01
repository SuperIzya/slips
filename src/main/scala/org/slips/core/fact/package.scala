package org.slips.core

package object fact {
  type ScalarFact = [t] =>> Fact.Val[t] =:= Fact[t]
  type TupleFact  = [x <: NonEmptyTuple] =>> Fact.Val[x] =:= Fact.TMap[x]

}
