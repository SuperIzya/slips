package org.slips.core.build.strategy

import org.slips.core.Fact
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec

/** Selection of predicates from condition of the rule */
sealed trait PredicateSelection {
  def selectPredicates(
    initialSources: Set[Fact[_]],
    allPredicates: Map[String, List[Predicate]]
  ): (Set[Fact[_]], Set[Predicate])
}

object PredicateSelection {

  /** Keep all predicates */
  case object Keep extends PredicateSelection {
    override def selectPredicates(
      initialSources: Set[Fact[_]],
      allPredicates: Map[String, List[Predicate]]
    ): (Set[Fact[_]], Set[Predicate]) = {
      val allP = allPredicates.values.toList.flatten.toSet
      allP.foldLeft(initialSources)(_ ++ _.sources) → allP
    }
  }

  /**
    * Don't use predicates on facts not involved in output
    */
  case object Clean extends PredicateSelection {
    override def selectPredicates(
      initialSources: Set[Fact[_]],
      allPredicates: Map[String, List[Predicate]]
    ): (Set[Fact[_]], Set[Predicate]) = {
      @tailrec
      def selectPredicates(
        toCheck: List[Predicate],
        predicates: Set[Predicate] = Set.empty,
        sources: Set[Fact[_]] = Set.empty
      ): (Set[Fact[_]], Set[Predicate]) = {
        toCheck match {
          case List(head, tail: _*) if !predicates.contains(head) ⇒
            val newSources         = head.sources.filter(!sources.contains(_))
            val ps: Set[Predicate] = newSources.flatMap(s ⇒ allPredicates(s.signature))
            selectPredicates(
              tail.toList ++ (ps -- predicates),
              predicates + head,
              sources ++ newSources
            )
          case List(_, tail: _*)                                  ⇒ selectPredicates(tail.toList, predicates, sources)
          case Nil                                                ⇒ (sources, predicates)
        }
      }

      selectPredicates(
        initialSources.flatMap(s ⇒ allPredicates(s.signature)).toList
      )
    }
  }
}
