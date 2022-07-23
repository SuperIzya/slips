package org.slips.core.build

import cats.data.State
import cats.syntax.traverse.*
import org.slips.Environment
import org.slips.core.TypeOps
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Parser
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import scala.annotation.tailrec

object Builder {

  case class Chain[T, F <: Fact.Val[T]](sources: F, predicates: Set[Predicate])

  def apply[T](condition: Condition[T])(using T: TypeOps[T]): Environment ?=> Unit = env ?=> {
    val (sources: Set[Condition.Source[_]], predicates) = sourcesAndPredicates(condition)
    val buildNet                                        = for {
      _ <- sources.toList.traverse(_.build)
    } yield ()
  }

  def sourcesAndPredicates[T](
    condition: Condition[T]
  )(
    using T: TypeOps[T]
  ): Environment ?=> (Set[Condition.Source[_]], Map[String, Predicate]) = (env: Environment) ?=> {
    val (Parser.Context(predicates, allSources), result) = Parser(condition)
    @tailrec
    def collectPredicates(p: List[Predicate], res: Map[String, List[Predicate]]): Map[String, List[Predicate]] = {
      p match {
        case Nil       => res
        case h :: tail =>
          val next = h.sources.map(_.signature).foldLeft(res)((m, s) => m + (s -> (h +: m.getOrElse(s, List.empty))))
          collectPredicates(
            tail,
            next
          )
      }
    }

    val collectedP = collectPredicates(predicates.toList, Map.empty).withDefaultValue(List.empty)

    val (sources, selectedP) = env
      .predicateSelectionStrategy
      .selectPredicates(
        T.predecessors(result).toSet,
        collectedP
      )

    val allSourcesMap = allSources.map(x => x.signature -> x).toMap
    sources.map(_.signature).map(allSourcesMap(_)) -> selectedP

  }
}
