package org.slips.core.build

import cats.Semigroup
import cats.implicits.*
import cats.syntax.*
import org.slips.Env
import org.slips.Signature
import org.slips.core.conditions.Predicate
import org.slips.core.fact.*

case class AlphaPredicate private[slips] (
  source: String,
  facts: Set[Fact.Alpha[?]],
  predicate: Predicate
)

object AlphaPredicate {
  def apply(p: Predicate): Env[Option[AlphaPredicate]] = env ?=> {
    val signature = env.signatureStrategy(p.signature)
    Predicate
      .IsAlpha
      .unapply(p)
      .map { case (p, src) => AlphaPredicate(signature, p.facts.map(_.asInstanceOf[Fact.Alpha[?]]), p) }
  }

  given Semigroup[AlphaPredicate] = Semigroup
    .instance((a, b) => AlphaPredicate(a.source, a.facts ++ b.facts, a.predicate))
}
