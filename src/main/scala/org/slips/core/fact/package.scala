package org.slips.core

package object fact {
  type ScalarFact = [x] =>> Fact.Val[x] =:= Fact[x]
  type TupleFact  = [x <: NonEmptyTuple] =>> Fact.Val[x] =:= Fact.TMap[x]
}
