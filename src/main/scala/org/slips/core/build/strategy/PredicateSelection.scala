package org.slips.core.build.strategy

import org.slips.Env
import org.slips.Environment
import org.slips.core.build.*
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.Predicate
import org.slips.core.fact.*
import org.slips.core.network.alpha.AlphaNode
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Selection of predicates from condition of the rule */
trait PredicateSelection {
  def selectPredicatesAndSources[T: FactOps](
    initial: Fact.Val[T],
    allFacts: AllFacts
  ): SelectedPredicatesAndSources
}

object PredicateSelection {

  def select[T](
    initial: Fact.Val[T],
    allFacts: AllFacts
  )(using T: FactOps[T]): Env[SelectedPredicatesAndSources] =
    env ?=> env.predicateSelectionStrategy.selectPredicatesAndSources(initial, allFacts)

  /** Keep all predicates */
  case object Keep extends PredicateSelection {
    override def selectPredicatesAndSources[T: FactOps](
      initial: Fact.Val[T],
      allFacts: AllFacts
    ): SelectedPredicatesAndSources = {

      collectPredicates(
        allFacts.values.flatten.toList,
        SelectedPredicatesAndSources(initial)
      )
    }

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
            collectPredicates(left :: right :: next, collected.withPredicate(head))
          case _                          =>
            collectPredicates(next, collected.withPredicate(head))
        }
      case Nil          => collected
    }
  }

  /**
    * Discard predicates and facts not involved in output
    * {{{
    * val condition = for {
    *   f <- all[Fruit]                    // 1
    *   v <- all[Vegetable]                // 2
    *   _ <- v.value(_.name) =:= "tomato"  // 3
    *   _ <- f.value(_.name) =!= "apple"   // 4
    * } yield v
    * }}}
    * lines 4 & 1 will be discarded by `parse` stage.
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

    override def selectPredicatesAndSources[T: FactOps](
      initial: Fact.Val[T],
      allFacts: AllFacts
    ): SelectedPredicatesAndSources = {
      val res = processPredicates(
        allFacts.values.flatten.toList,
        SelectedPredicatesAndSources
          .empty
          .copy(
            facts = initial.predecessors.toSet,
            sources = initial.alphaSources.map(_.signature)
          )
      )

      processDiscarded(res)
    }

    extension (q: Queue[Predicate]) {
      private inline def deq(selected: SelectedPredicatesAndSources): SelectedPredicatesAndSources =
        q.dequeueOption match {
          case Some((pd, qu)) => collectSources(selected, pd, qu)
          case None           => selected
        }
    }

    extension (facts: Set[Fact[?]]) {
      private inline def allPredecessors: Set[Fact[?]] = facts.flatMap(f => f +: f.predecessors)
    }

    private def processDiscarded(selected: SelectedPredicatesAndSources): SelectedPredicatesAndSources = {
      if (selected.discarded.isEmpty) selected
      else selected.discarded.foldLeft(selected.copy(discarded = Set.empty))(collectSources(_, _))
    }

    @tailrec
    private def collectSources(
      col: SelectedPredicatesAndSources,
      p: Predicate,
      queue: Queue[Predicate] = Queue.empty
    ): SelectedPredicatesAndSources = {
      p match {
        case _ if col.facts.intersect(p.facts.allPredecessors).isEmpty                              =>
          queue.deq(col.withDiscard(p))
        case Predicate.Test(_, _, _)                                                                =>
          queue.deq(col.withPredicate(p))
        case Predicate.Not(pred)                                                                    =>
          collectSources(col.withPredicate(p), pred, queue)
        case Predicate.Or(left, right)                                                              =>
          collectSources(col.withPredicate(p), left, queue.enqueue(right))
        case Predicate.And(left, right) if col.facts.intersect(left.facts.allPredecessors).nonEmpty =>
          collectSources(col, left, queue.enqueue(right))
        case Predicate.And(l, right)                                                                =>
          collectSources(col.withDiscard(l), right, queue)
      }
    }

    @tailrec
    private def processPredicates(
      toCheck: List[Predicate],
      collected: SelectedPredicatesAndSources
    ): SelectedPredicatesAndSources = {
      val result = toCheck.foldLeft(collected)(collectSources(_, _))
      if (result.discarded == collected.discarded) result
      else processPredicates(result.discarded.toList, result)
    }
  }
}
