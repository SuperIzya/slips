package org.slips.core.build

import cats.Semigroup
import cats.implicits.*
import cats.syntax.*
import org.slips.Env
import org.slips.Signature
import org.slips.core.conditions.Predicate
import org.slips.core.fact.*

private[slips] case class BuildPredicate(
  facts: Set[Fact.Source[?]],
  arity: Int,
  predicate: Predicate
)

private[slips] object BuildPredicate {
  given Semigroup[BuildPredicate] = Semigroup
    .instance((a, b) => BuildPredicate(a.facts ++ b.facts, a.arity, a.predicate))
}
