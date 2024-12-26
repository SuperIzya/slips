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

    def invert: Env[Chain] = {
      val next = nextPredicate(None)
      invertNext(next)
    }

    private def nextPredicate(newTail: Option[Predicates]): Env[Predicates] =
      Predicates(
        predicates = predicates,
        tail = newTail,
        facts = facts -- tail.headOption.fold(Set.empty)(_.facts),
        allPredicates = predicates.map(_.predicate.signature.compute) ++ newTail.fold(Set.empty)(_.allPredicates)
      )

    private inline def invertNext(next: => Predicates): Env[Chain] =
      tail.headOption match {
        case Some(value) => value.invert(next)
        case None        => next
      }

    private final def invert(newTail: Predicates): Env[Chain] =
      invertNext(nextPredicate(Some(newTail)))
  }

  case class Combine(left: Chain, right: Chain) extends Chain {
    def tail: Iterable[Chain] = Seq(left, right)

    def facts: Set[Fact.Source[?]] = left.facts ++ right.facts

    def predicates: Set[BuildPredicate] = Set.empty

    def allPredicates: Set[String] = left.allPredicates ++ right.allPredicates
  }

  extension (chain: Predicates) {
    def appendPredicate(predicate: BuildPredicate): Env[Predicates] = env ?=> {
      chain.copy(
        predicates = chain.predicates + predicate,
        allPredicates = chain.allPredicates + predicate.predicate.signature.compute
      )
    }
  }

  given Ordering[Chain] = (x, y) => x.facts.sizeCompare(y.facts)

  def apply(predicate: BuildPredicate, facts: Set[Fact.Source[?]]): Env[Predicates] =
    apply(predicate, facts, None)

  def apply(predicate: BuildPredicate, facts: Set[Fact.Source[?]], tail: Predicates): Env[Predicates] =
    apply(predicate, facts, Option(tail))

  def apply(predicate: BuildPredicate, facts: Set[Fact.Source[?]], tail: Option[Predicates]): Env[Predicates] =
    Predicates(
      Set(predicate),
      tail,
      facts,
      tail.view.toSet.flatMap(_.allPredicates) + predicate.predicate.signature.compute
    )
}
