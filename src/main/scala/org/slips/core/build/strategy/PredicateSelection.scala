package org.slips.core.build.strategy

import org.slips.Env
import org.slips.core.build.*
import org.slips.core.build.strategy.PredicateSelection.ValidatedRes
import org.slips.core.conditions.Predicate
import org.slips.core.conditions.PredicateSyntax
import org.slips.core.fact.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Selection of predicates from condition of the rule */
trait PredicateSelection {
  def selectPredicatesAndSources[T: FactOps](initial: Fact.Val[T], allFacts: AllFacts): ValidatedRes
}

object PredicateSelection {

  type ValidatedRes = Result[SelectedPredicatesAndSources]

  def select[T](initial: Fact.Val[T], allFacts: AllFacts)(using T: FactOps[T]): Env[ValidatedRes] =
    env ?=> env.predicateSelectionStrategy.selectPredicatesAndSources(initial, allFacts)

  /** Keep all predicates */
  case object Keep extends PredicateSelection {
    import Predicate.*
    def selectPredicatesAndSources[T: FactOps](initial: Fact.Val[T], allFacts: AllFacts): ValidatedRes =
      Result.result(
        collectPredicates(
          allFacts.values.flatten.toSet,
          SelectedPredicatesAndSources(initial)
        )
      )

    @tailrec
    private def collectPredicates(
      predicates: Set[Predicate],
      collected: SelectedPredicatesAndSources
    ): SelectedPredicatesAndSources = predicates.headOption match {
      case Some(head) =>
        head match {
          case left && right =>
            collectPredicates((predicates.tail + left) + right, collected)
          case left || right =>
            collectPredicates((predicates.tail + left) + right, collected.withPredicate(head))
          case _             =>
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
    import Predicate.*

    def selectPredicatesAndSources[T: {FactOps as T}](
      initial: Fact.Val[T],
      allFacts: AllFacts
    ): ValidatedRes = {
      val initialSources = T.sources(initial)
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
      private inline def deq(selected: SelectedPredicatesAndSources): ValidatedRes =
        q.dequeueOption match {
          case Some((pd, qu)) => collectSources(selected, pd, qu)
          case None           => Result.result(selected)
        }
    }

    private def processDiscarded(selected: SelectedPredicatesAndSources): ValidatedRes = {
      if (selected.discarded.isEmpty) Result.result(selected)
      else
        selected
          .discarded
          .foldLeft(Result.result(selected.copy(discarded = Set.empty))) { (col, dis) =>
            col.flatMap(collectSources(_, dis))
          }
    }

    @tailrec
    private def collectSources(
      col: SelectedPredicatesAndSources,
      p: Predicate,
      queue: Queue[Predicate] = Queue.empty
    ): ValidatedRes = {
      p match {
        case _ if col.facts.intersect(p.facts).isEmpty                 =>
          queue.deq(col.withDiscard(p))
        case Predicate.Test(_, _, _)                                   =>
          queue.deq(col.withPredicate(p))
        case !(pred)                                                   =>
          collectSources(col.withPredicate(p), pred, queue)
        case left || right                                             =>
          collectSources(col.withPredicate(p), left, queue.enqueue(right))
        case left && right if col.facts.intersect(left.facts).nonEmpty =>
          collectSources(col, left, queue.enqueue(right))
        case l && right                                                =>
          collectSources(col.withDiscard(l), right, queue)
        case _                                                         => Result.errorMsg(s"Unexpected predicate $p")
      }
    }

    @tailrec
    private def processPredicates(
      toCheck: List[Predicate],
      collected: SelectedPredicatesAndSources,
      warnings: List[Warning] = List.empty
    ): ValidatedRes = {

      val result: ValidatedRes = toCheck
        .foldLeft(Result.result(collected)) { (col, p) => col.flatMap(collectSources(_, p)) }
        .withMaybeWarnings(warnings)

      result match {
        case Result.Pure(res) if res.discarded == collected.discarded                   => Result.result(res)
        case Result.WithWarnings(res, warnings) if res.discarded == collected.discarded =>
          Result.WithWarnings(res, warnings)
        case Result.Error(error)                                                        => Result.Error(error)
        case Result.WithWarnings(res, warnings)                                         =>
          processPredicates(res.discarded.toList, res, warnings.toList)
        case Result.Pure(res) => processPredicates(res.discarded.toList, res)
      }
    }
  }
}
