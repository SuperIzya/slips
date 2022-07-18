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
  ): (Set[Fact[_]], Map[String, Predicate])
}

object PredicateSelection {

  /** Keep all predicates */
  case object Keep extends PredicateSelection {
    override def selectPredicates(
      initialSources: Set[Fact[_]],
      allPredicates: Map[String, List[Predicate]]
    ): (Set[Fact[_]], Map[String, Predicate]) = {
      @tailrec
      def collectPredicates(
        predicates: List[Predicate],
        collectedS: Set[Fact[_]],
        collectedP: Map[String, Predicate]
      ): (Set[Fact[_]], Map[String, Predicate]) = predicates match {
        case head :: next =>
          head match {
            case Predicate.And(left, right) => collectPredicates(left :: right :: next, collectedS, collectedP)
            case Predicate.Or(left, right)  =>
              collectPredicates(left :: right :: next, collectedS, collectedP + (head.signature -> head))
            case _ => collectPredicates(next, collectedS ++ head.sources, collectedP + (head.signature -> head))
          }
        case Nil          => (collectedS, collectedP)
      }

      collectPredicates(allPredicates.values.toList.flatten, initialSources, Map.empty)
    }
  }

  /**
    * Don't use predicates on facts not involved in output
    */
  case object Clean extends PredicateSelection {
    override def selectPredicates(
      initialSources: Set[Fact[_]],
      allPredicates: Map[String, List[Predicate]]
    ): (Set[Fact[_]], Map[String, Predicate]) = {
      @tailrec
      def selectPredicates(
        toCheck: List[Predicate],
        predicates: Map[String, Predicate] = Map.empty,
        sources: Set[Fact[_]] = Set.empty
      ): (Set[Fact[_]], Map[String, Predicate]) = {
        toCheck match {
          case List(head, tail: _*) if !predicates.contains(head.signature) =>
            @tailrec
            def collectSources(
              p: Predicate,
              collectedS: Set[Fact[_]],
              collectedP: Map[String, Predicate],
              queue: Queue[Predicate] = Queue.empty
            ): (Set[Fact[_]], Map[String, Predicate]) = {
              inline def dequeue(
                newSources: Set[Fact[_]],
                newPredicates: Map[String, Predicate]
              ): (Set[Fact[_]], Map[String, Predicate]) =
                queue.dequeueOption match {
                  case Some((pd, qu)) => collectSources(pd, newSources, newPredicates, qu)
                  case None           => newSources -> newPredicates
                }
              p match {
                case Predicate.Test(_, _, rep) if initialSources.intersect(rep.sources.toSet).nonEmpty =>
                  val newSources    = collectedS ++ rep.sources
                  val newPredicates = collectedP + (p.signature -> p)
                  dequeue(newSources, newPredicates)
                case Predicate.Test(_, _, _)                                                           =>
                  dequeue(collectedS, collectedP)
                case Predicate.Not(pred) => collectSources(pred, collectedS, collectedP + (p.signature -> p), queue)
                case Predicate.Or(left, right)                                                     =>
                  collectSources(left, collectedS, collectedP + (p.signature -> p), queue.enqueue(right))
                case Predicate.And(left, right) if initialSources.intersect(left.sources).nonEmpty =>
                  collectSources(left, collectedS, collectedP, queue.enqueue(right))
                case Predicate.And(_, right) if initialSources.intersect(right.sources).nonEmpty   =>
                  collectSources(right, collectedS, collectedP, queue)
                case _                                                                             =>
                  dequeue(collectedS, collectedP)
              }
            }
            val (newSources, newPredicates) = collectSources(head, sources, predicates)
            selectPredicates(
              tail.toList,
              newPredicates,
              newSources
            )
          case List(_, tail: _*)                                            =>
            selectPredicates(tail.toList, predicates, sources)
          case Nil                                                          => (sources, predicates)
        }
      }

      selectPredicates(
        initialSources.flatMap(s => allPredicates(s.signature)).toList
      )
    }
  }
}
