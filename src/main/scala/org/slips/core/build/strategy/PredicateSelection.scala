package org.slips.core.build.strategy

import org.slips.Env
import org.slips.Environment
import org.slips.core.TypeOps
import org.slips.core.build.*
import org.slips.core.conditions.Condition.Source
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Selection of predicates from condition of the rule */
trait PredicateSelection {
  def selectPredicatesAndSources[T: TypeOps](
    initial: Fact.Val[T],
    predicates: PredicateSelectionMap
  ): SelectedPredicatesAndSources
}

object PredicateSelection {

  def select[T: TypeOps](initial: Fact.Val[T], predicates: PredicateSelectionMap): Env[SelectedPredicatesAndSources] =
    env ?=> env.predicateSelectionStrategy.selectPredicatesAndSources(initial, predicates)

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

    override def selectPredicatesAndSources[T: TypeOps](
      initial: Fact.Val[T],
      predicates: PredicateSelectionMap
    ): SelectedPredicatesAndSources = {

      collectPredicates(
        predicates.values.toList.flatten,
        SelectedPredicatesAndSources(initial)
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
        case Some((pd, qu)) => collectSources(selected, pd, qu)
        case None           => selected
      }
    }

    @tailrec
    private def collectSources(
      col: SelectedPredicatesAndSources,
      p: Predicate,
      queue: Queue[Predicate] = Queue.empty
    ): SelectedPredicatesAndSources = {
      p match {
        case Predicate.Test(_, _, rep) if col.facts.intersect(rep.sourceFacts).nonEmpty   =>
          queue.deq(
            col.copy(
              sources = col.sources ++ rep.sources,
              predicates = col.addPredicate(p),
              facts = col.facts ++ rep.sourceFacts
            )
          )
        case Predicate.Test(_, _, _)                                                      =>
          queue.deq(col.withDiscard(p))
        case Predicate.Not(pred) if col.facts.intersect(pred.sourceFacts).nonEmpty        =>
          collectSources(col.withPredicate(p), pred, queue)
        case Predicate.Not(_)                                                             =>
          queue.deq(col.withDiscard(p))
        case Predicate.Or(left, right)                                                    =>
          collectSources(col.withPredicate(p), left, queue.enqueue(right))
        case Predicate.And(left, right) if col.facts.intersect(left.sourceFacts).nonEmpty =>
          collectSources(col, left, queue.enqueue(right))
        case Predicate.And(l, right) if col.facts.intersect(right.sourceFacts).nonEmpty   =>
          collectSources(col.withDiscard(l), right, queue)
        case _                                                                            =>
          queue.deq(col.withDiscard(p))
      }
    }

    private def selectPredicates(
      toCheck: List[Predicate],
      collected: SelectedPredicatesAndSources
    ): SelectedPredicatesAndSources = {
      toCheck.foldLeft(collected)(collectSources(_, _))
    }

    @tailrec
    private def processPredicates(
      toCheck: List[Predicate],
      collected: SelectedPredicatesAndSources
    ): SelectedPredicatesAndSources = {
      val result = selectPredicates(toCheck, collected)
      if (result.discarded == collected.discarded) result
      else processPredicates(result.discarded.toList, result.copy(discarded = Set.empty))
    }
    override def selectPredicatesAndSources[T](
      initial: Fact.Val[T],
      predicates: PredicateSelectionMap
    )(using T: TypeOps[T]
    ): SelectedPredicatesAndSources = {

      val sources = T.sources(initial)

      val res = selectPredicates(
        predicates.values.flatten.toList,
        SelectedPredicatesAndSources
          .empty
          .copy(
            facts = T.sourceFacts(initial),
            sources = sources
          )
      )

      processPredicates(res.discarded.toList, res)
    }
  }
}
