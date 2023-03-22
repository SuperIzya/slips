package org.slips.core.build

import cats.Semigroup
import cats.implicits.*
import cats.syntax.*
import org.slips.core.fact.*
import org.slips.core.predicates.Predicate

case class AlphaPredicate private[slips] (
  source: String,
  facts: Set[Fact.Alpha[_]],
  predicate: Predicate
)

object AlphaPredicate {
  def apply(p: Predicate): Option[AlphaPredicate] = Predicate
    .IsAlpha
    .unapply(p)
    .map { case (p, src) => AlphaPredicate(src.signature, p.facts.map(_.asInstanceOf[Fact.Alpha]), p) }

  given Semigroup[AlphaPredicate] = Semigroup
    .instance((a, b) => AlphaPredicate(a.source, a.facts + b.facts, a.predicate))
}
