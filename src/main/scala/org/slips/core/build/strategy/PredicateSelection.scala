package org.slips.core.build.strategy

import org.slips.Env
import org.slips.Environment
import org.slips.core.build.*
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.Predicate
import org.slips.core.fact.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Selection of predicates from condition of the rule */
trait PredicateSelection {
  def selectPredicatesAndSources[T: FactOps](
    initial: Fact.Val[T],
    allFacts: AllFacts
  ): Either[String, SelectedPredicatesAndSources]
}

object PredicateSelection {

  def select[T](
    initial: Fact.Val[T],
    allFacts: AllFacts
  )(using T: FactOps[T]): Env[Either[String, SelectedPredicatesAndSources]] =
    env ?=> env.predicateSelectionStrategy.selectPredicatesAndSources(initial, allFacts)

  /** Keep all predicates */
  case object Keep extends PredicateSelection {
    override def selectPredicatesAndSources[T: FactOps](
      initial: Fact.Val[T],
      allFacts: AllFacts
    ): Either[String, SelectedPredicatesAndSources] = {

      Right(
        collectPredicates(
          allFacts.values.flatten.toSet,
          SelectedPredicatesAndSources(initial)
        )
      )
    }

    @tailrec
    private def collectPredicates(
      predicates: Set[Predicate],
      collected: SelectedPredicatesAndSources
    ): SelectedPredicatesAndSources = predicates.headOption match {
      case Some(head) =>
        head match {
          case Predicate.And(left, right) =>
            collectPredicates((predicates.tail + left) + right, collected)
          case Predicate.Or(left, right)  =>
            collectPredicates((predicates.tail + left) + right, collected.withPredicate(head))
          case _                          =>
            collectPredicates(predicates.tail, collected.withPredicate(head))
        }
      case None       => collected
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
    ): Either[String, SelectedPredicatesAndSources] = {
      val initialSources = initial.sources
      processPredicates(
        allFacts.values.flatten.toList,
        SelectedPredicatesAndSources
          .empty
          .copy(
            facts = initialSources,
            signatures = initialSources.map(_.signature)
          )
      ).flatMap(processDiscarded)
    }

    extension (q: Queue[Predicate]) {
      private inline def deq(selected: SelectedPredicatesAndSources): Either[String, SelectedPredicatesAndSources] =
        q.dequeueOption match {
          case Some((pd, qu)) => collectSources(selected, pd, qu)
          case None           => Right(selected)
        }
    }

    private def processDiscarded(
      selected: SelectedPredicatesAndSources
    ): Either[String, SelectedPredicatesAndSources] = {
      if (selected.discarded.isEmpty) Right(selected)
      else
        selected
          .discarded
          .foldLeft[Either[String, SelectedPredicatesAndSources]](Right(selected.copy(discarded = Set.empty))) {
            (col, dis) => col.flatMap(collectSources(_, dis))
          }
    }

    @tailrec
    private def collectSources(
      col: SelectedPredicatesAndSources,
      p: Predicate,
      queue: Queue[Predicate] = Queue.empty
    ): Either[String, SelectedPredicatesAndSources] = {
      p match {
        case _ if col.facts.intersect(p.facts).isEmpty                              =>
          queue.deq(col.withDiscard(p))
        case Predicate.Test(_, _, _)                                                =>
          queue.deq(col.withPredicate(p))
        case Predicate.Not(pred)                                                    =>
          collectSources(col.withPredicate(p), pred, queue)
        case Predicate.Or(left, right)                                              =>
          collectSources(col.withPredicate(p), left, queue.enqueue(right))
        case Predicate.And(left, right) if col.facts.intersect(left.facts).nonEmpty =>
          collectSources(col, left, queue.enqueue(right))
        case Predicate.And(l, right)                                                =>
          collectSources(col.withDiscard(l), right, queue)
        case _                                                                      => Left(s"Unexpected predicate $p")
      }
    }

    @tailrec
    private def processPredicates(
      toCheck: List[Predicate],
      collected: SelectedPredicatesAndSources
    ): Either[String, SelectedPredicatesAndSources] = {

      val result = toCheck.foldLeft[Either[String, SelectedPredicatesAndSources]](Right(collected)) { (col, p) =>
        col.flatMap(collectSources(_, p))
      }
      result match {
        case Left(value)                                          => Left(value)
        case Right(res) if (res.discarded == collected.discarded) => Right(res)
        case Right(res)                                           => processPredicates(res.discarded.toList, res)
      }
    }
  }
}
