package org.slips.core.build

import cats.data.State
import cats.syntax.traverse.*
import org.slips.Env
import org.slips.Environment
import org.slips.core.TypeOps
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.Parser
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec

object Builder {

  type PredicateMap = Map[Fact.Source[_], Set[Predicate]]

  case class SelectedPredicatesAndSources(
    predicates: PredicateMap,
    sources: Set[Source[_]],
    facts: Set[Fact.Source[_]],
    discarded: Set[Predicate]
  ) {
    import SelectedPredicatesAndSources._
    def addPredicate(p: Predicate): PredicateMap = {
      addToMap(predicates, p)
    }

    def withPredicate(p: Predicate): SelectedPredicatesAndSources = copy(
      predicates = addPredicate(p),
      facts = facts ++ p.sourceFacts
    )

    def withDiscard(p: Predicate): SelectedPredicatesAndSources = copy(discarded = discarded + p)
  }

  object SelectedPredicatesAndSources {
    val empty: SelectedPredicatesAndSources = SelectedPredicatesAndSources(Map.empty, Set.empty, Set.empty, Set.empty)
    private inline def addToMap(map: PredicateMap, p: Predicate): PredicateMap          = {
      map ++ p.facts.flatMap(f => f.sourceFacts).map { f => f -> (map.getOrElse(f, Set.empty) + p) }
    }
    def apply[T](start: Fact.Val[T])(using T: TypeOps[T]): SelectedPredicatesAndSources = {
      SelectedPredicatesAndSources(
        Map.empty,
        T.sources(start),
        T.sourceFacts(start),
        Set.empty
      )
    }
  }

  def apply[T](condition: Condition[T])(using T: TypeOps[T]): Env[Unit] = env ?=> {
    val SelectedPredicatesAndSources(predicates, sources, facts, _) = sourcesAndPredicates(condition)
    predicates.values.flatten.groupBy(_.sourceFacts.size)
    val buildNet                                                    = for {
      _ <- sources.toList.traverse(_.build)
    } yield ()
  }

  def sourcesAndPredicates[T](condition: Condition[T])(using T: TypeOps[T]): Env[SelectedPredicatesAndSources] = {
    val (Parser.Context(predicates, _), result) = Parser(condition)

    PredicateSelection.select(
      result,
      predicates.flatMap(f => f.sourceFacts.map(_ -> f)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    )
  }
}
