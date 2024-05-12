package org.slips.core.build

import cats.Semigroup
import cats.implicits.*
import cats.syntax.*
import org.slips.Env
import org.slips.Signature
import org.slips.core.conditions.Predicate
import org.slips.core.fact.*

case class BuildPredicate private[slips](
  facts: Map[Fact.Source[?], Set[Fact.Source[?]]],
  arity: Int,
  predicate: Predicate
)

object BuildPredicate {
}
