package org.slips.core.build

import cats.data.State
import cats.syntax.traverse.*
import org.slips.Environment
import org.slips.core.TypeOps
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.Parser
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.Env
import scala.annotation.tailrec

object Builder {

  type PredicateMap = Map[Fact[_], Set[Predicate]]

  case class SelectedPredicatesAndSources(predicates: PredicateMap, sources: Set[Source[_]], facts: Set[Fact[_]]) {
    def addPredicate(p: Predicate): PredicateMap  = {
      predicates ++ p.facts.flatMap(f => f.predecessors + f).map { f => f -> (predicates.getOrElse(f, Set.empty) + p) }
    }
    def withPredicate(p: Predicate): SelectedPredicatesAndSources = copy(predicates = addPredicate(p))
  }

  object SelectedPredicatesAndSources {
    def apply[T](start: Fact.Val[T])(using T: TypeOps[T]): SelectedPredicatesAndSources = {
      SelectedPredicatesAndSources(
        Map.empty,
        T.sources(start),
        T.predecessors(start)
      )
    }
    def empty = SelectedPredicatesAndSources(Map.empty, Set.empty, Set.empty)
  }

  def apply[T](condition: Condition[T])(using T: TypeOps[T]): Env[Unit] = env ?=> {
    val SelectedPredicatesAndSources(predicates, sources, facts) = sourcesAndPredicates(condition)
    val buildNet                                          = for {
      _ <- sources.toList.traverse(_.build)
    } yield ()
  }

  def sourcesAndPredicates[T](condition: Condition[T])(using T: TypeOps[T]): Env[SelectedPredicatesAndSources] = {
    val (Parser.Context(predicates, allSources), result) = Parser(condition)
    @tailrec
    def collectPredicates(p: List[Predicate], res: SelectedPredicatesAndSources): SelectedPredicatesAndSources = {
      p match {
        case Nil       => res
        case h :: tail =>
          collectPredicates(tail, res.withPredicate(h))
      }
    }
    val collected = collectPredicates(predicates.toList, SelectedPredicatesAndSources(result))
    PredicateSelection.select(collected)
  }
}
