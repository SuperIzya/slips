package org.slips.core.build.strategy

import org.slips.core.Fact
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec
import scala.collection.immutable.Queue

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
            @tailrec
            def collectSources(
              p: Predicate,
              collectedS: Set[Fact[_]],
              collectedP: Set[Predicate],
              queue: Queue[Predicate] = Queue.empty
            ): (Set[Fact[_]], Set[Predicate]) = {
              inline def dequeue(newSources: Set[Fact[_]], newPredicates: Set[Predicate]): (Set[Fact[_]], Set[Predicate]) =
                queue.dequeueOption match {
                  case Some((pd, qu)) ⇒ collectSources(pd, newSources, newPredicates, qu)
                  case None           ⇒ newSources → newPredicates
                }
              p match {
                case Predicate.Test(_, _, rep) if initialSources.intersect(rep.sources.toSet).nonEmpty ⇒
                  val newSources    = collectedS ++ rep.sources
                  val newPredicates = collectedP + p
                  dequeue(newSources, newPredicates)
                case Predicate.Test(_, _, _)                                                           ⇒
                  dequeue(collectedS, collectedP)
                case Predicate.And(left, right)                                                        ⇒
                  collectSources(left, collectedS, collectedP, queue.enqueue(right))
                case Predicate.Not(pred)       ⇒ collectSources(pred, collectedS, collectedP + p, queue)
                case Predicate.Or(left, right) ⇒
                  collectSources(left, collectedS, collectedP + p, queue.enqueue(right))
              }
            }
            val (newSources, newPredicates) = collectSources(head, sources, predicates)
            selectPredicates(
              tail.toList,
              newPredicates,
              newSources
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
