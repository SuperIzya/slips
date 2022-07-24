package org.slips.core.build.strategy

import org.slips.Env
import org.slips.Environment
import org.slips.core.build.Builder.SelectedPredicatesAndSources
import org.slips.core.conditions.Condition.Source
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Selection of predicates from condition of the rule */
sealed trait PredicateSelection {
  def selectPredicatesAndSources(initial: SelectedPredicatesAndSources): SelectedPredicatesAndSources
}

object PredicateSelection {

  def select(initial: SelectedPredicatesAndSources): Env[SelectedPredicatesAndSources] =
    env ?=> env.predicateSelectionStrategy.selectPredicatesAndSources(initial)

  /** Keep all predicates */
  case object Keep extends PredicateSelection {
    @tailrec
    private def collectPredicates(
      predicates: List[Predicate],
      collected: SelectedPredicatesAndSources
    ): SelectedPredicatesAndSources = predicates match {
      case head :: next =>
        head match {
          case Predicate.And(left, right) =>
            collectPredicates(left :: right :: next, collected)
          case Predicate.Or(left, right)  =>
            collectPredicates(
              left :: right :: next,
              collected.withPredicate(head)
            )
          case _                          =>
            collectPredicates(
              next,
              collected.copy(
                sources = collected.sources ++ head.sources,
                predicates = collected.addPredicate(head)
              )
            )
        }
      case Nil          => collected
    }

    override def selectPredicatesAndSources(initial: SelectedPredicatesAndSources): SelectedPredicatesAndSources = {

      collectPredicates(
        initial.predicates.values.toList.flatten,
        initial.copy(predicates = Map.empty)
      )
    }
  }

  /**
    * Don't use predicates on facts not involved in output
    * {{{
    * val condition = for {
    *   f <- all[Fruit]                    // 1
    *   v <- all[Vegetable]                // 2
    *   _ <- v.value(_.name) =:= "tomato"  // 3
    *   _ <- f.value(_.name) =!= "apple"   // 4
    * } yield v
    * }}}
    * lines 4 & 1 will not be passed by parsing stage.
    *
    * In case
    * {{{
    * val condition = for {
    *   f <- all[Fruit]                             // 1
    *   v <- all[Vegetable]                         // 2
    *   _ <- v.value(_.name) =:= "tomato"           // 3
    *   _ <- f.value(_.name) =!= "apple"            // 4
    *   _ <- v.value(_.color) =:= f.value(_.color)  // 5
    * } yield v
    * }}}
    * all lines will be parsed since line 5 ties fact `f` &
    * fact `v`
    */
  case object Clean extends PredicateSelection {

    extension (q: Queue[Predicate]) {
      private inline def deq(selected: SelectedPredicatesAndSources): SelectedPredicatesAndSources = q
        .dequeueOption match {
        case Some((pd, qu)) => collectSources(pd, selected, qu)
        case None           => selected
      }
    }

    @tailrec
    private def collectSources(
      p: Predicate,
      col: SelectedPredicatesAndSources,
      queue: Queue[Predicate] = Queue.empty
    ): SelectedPredicatesAndSources = {
      p match {
        case Predicate.Test(_, _, rep) if col.facts.intersect(rep.predecessors).nonEmpty =>
          queue.deq(
            col.copy(
              sources = col.sources ++ rep.sources,
              predicates = col.addPredicate(p),
              facts = col.facts ++ rep.predecessors
            )
          )
        case Predicate.Test(_, _, _)                                                     =>
          queue.deq(col)
        case Predicate.Not(pred)                                                         =>
          collectSources(pred, col.withPredicate(p), queue)
        case Predicate.Or(left, right)                                                   =>
          collectSources(left, col.withPredicate(p), queue.enqueue(right))
        case Predicate.And(left, right) if col.facts.intersect(left.facts).nonEmpty      =>
          collectSources(left, col, queue.enqueue(right))
        case Predicate.And(_, right) if col.facts.intersect(right.facts).nonEmpty        =>
          collectSources(right, col, queue)
        case _                                                                           =>
          queue.deq(col)
      }
    }
    @tailrec
    private def selectPredicates(
      toCheck: List[Predicate],
      collected: SelectedPredicatesAndSources
    ): SelectedPredicatesAndSources = {
      toCheck match {
        case List(head, tail: _*) =>
          val selected = collectSources(head, collected)
          selectPredicates(tail.toList, selected)
        case Nil                  => collected
      }
    }
    override def selectPredicatesAndSources(initial: SelectedPredicatesAndSources) = {

      val allPredicatesMap: Map[Source[_], Set[Predicate]] = initial
        .predicates
        .toSet
        .flatMap { case (fact, pred) => fact.sources.map(_ -> pred) }
        .toMap

      selectPredicates(
        initial.sources.flatMap { allPredicatesMap(_) }.toList,
        initial.copy(
          predicates = Map.empty,
          facts = initial.facts.flatMap(_.predecessors)
        )
      )
    }
  }
}
