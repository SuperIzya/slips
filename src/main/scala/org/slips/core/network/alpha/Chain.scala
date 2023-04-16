package org.slips.core.network.alpha

import org.slips.core.build.AlphaPredicate
import org.slips.core.fact.Fact
import scala.annotation.tailrec

private[network] sealed trait Chain {
  def predicates: Set[AlphaPredicate]
  def tail: Iterable[Chain]
  def facts: Set[Fact.Alpha[_]]
  def allPredicates: Set[String]
}

private[network] object Chain {

  case class Predicates(
    predicates: Set[AlphaPredicate],
    tail: Iterable[Predicates],
    facts: Set[Fact.Alpha[_]],
    allPredicates: Set[String]
  ) extends Chain {

    @tailrec
    final def invert(newTail: Option[Predicates] = None): Chain = {
      val tailM = tail.headOption
      val next  = Predicates(
        predicates = predicates,
        tail = newTail,
        facts = tailM.map(_.facts).fold(facts)(facts -- _),
        allPredicates = newTail
          .fold(predicates.map(_.predicate.signature))(_.allPredicates ++ predicates.map(_.predicate.signature))
      )

      if (tailM.isEmpty) next
      else tailM.get.invert(Some(next))
    }
  }

  case class Combine(
    left: Chain,
    right: Chain
  ) extends Chain {
    override def tail: Iterable[Chain] = Seq(left, right)

    override lazy val facts: Set[Fact.Alpha[_]] = left.facts ++ right.facts

    override def predicates: Set[AlphaPredicate] = Set.empty

    override lazy val allPredicates: Set[String] = left.allPredicates ++ right.allPredicates
  }

  extension (chain: Predicates) {
    def appendPredicate(predicate: AlphaPredicate, facts: Set[Fact.Alpha[_]]): Predicates = {
      chain.copy(
        predicates = chain.predicates + predicate,
        facts = chain.facts ++ facts,
        allPredicates = chain.allPredicates + predicate.predicate.signature
      )
    }
  }

  given Ordering[Chain] = (x, y) => x.facts.sizeCompare(y.facts)

  def apply(predicate: AlphaPredicate, facts: Set[Fact.Alpha[_]]): Predicates =
    apply(predicate, facts, None)

  def apply(predicate: AlphaPredicate, facts: Set[Fact.Alpha[_]], tail: Predicates): Predicates =
    apply(predicate, facts, Option(tail))

  def apply(predicate: AlphaPredicate, facts: Set[Fact.Alpha[_]], tail: Option[Predicates]): Predicates =
    Predicates(Set(predicate), tail, facts, tail.view.toSet.flatMap(_.allPredicates) + predicate.predicate.signature)

}
