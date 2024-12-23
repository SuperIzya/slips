package org.slips.core.network

import org.slips.Env
import org.slips.core.build.BuildPredicate
import org.slips.core.fact.Fact
import scala.annotation.tailrec

private[network] sealed trait Chain {
  def predicates: Set[BuildPredicate]
  def tail: Iterable[Chain]
  def facts: Set[Fact.Source[?]]
  def allPredicates: Set[String]
}

private[network] object Chain {

  case class Predicates(
    predicates: Set[BuildPredicate],
    tail: Iterable[Predicates],
    facts: Set[Fact.Source[?]],
    allPredicates: Set[String]
  ) extends Chain {

    @tailrec
    final def invert(newTail: Option[Predicates] = None): Env[Chain] = env ?=> {
      val tailM = tail.headOption
      val next  = Predicates(
        predicates = predicates,
        tail = newTail,
        facts = tailM.map(_.facts).fold(facts)(facts -- _),
        allPredicates = newTail.fold(predicates.map(_.predicate.signature).map(_.compute))(
          _.allPredicates ++ predicates.map(_.predicate.signature).map(_.compute)
        )
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

    override lazy val facts: Set[Fact.Source[?]] = left.facts ++ right.facts

    override def predicates: Set[BuildPredicate] = Set.empty

    override lazy val allPredicates: Set[String] = left.allPredicates ++ right.allPredicates
  }

  extension (chain: Predicates) {
    def appendPredicate(predicate: BuildPredicate, facts: Set[Fact.Source[?]]): Env[Predicates] = env ?=> {
      chain.copy(
        predicates = chain.predicates + predicate,
        facts = chain.facts ++ facts,
        allPredicates = chain.allPredicates + predicate.predicate.signature.compute
      )
    }
  }

  given Ordering[Chain] = (x, y) => x.facts.sizeCompare(y.facts)

  def apply(predicate: BuildPredicate, facts: Set[Fact.Source[?]]): Env[Predicates] =
    apply(predicate, facts, None)

  def apply(predicate: BuildPredicate, facts: Set[Fact.Source[?]], tail: Predicates): Env[Predicates] =
    apply(predicate, facts, Option(tail))

  def apply(predicate: BuildPredicate, facts: Set[Fact.Source[?]], tail: Option[Predicates]): Env[Predicates] = env ?=>
    {
      Predicates(
        Set(predicate),
        tail,
        facts,
        tail.view.toSet.flatMap(_.allPredicates) + predicate.predicate.signature.compute
      )
    }

}
