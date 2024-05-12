package org.slips.core.build

import cats.data.IndexedStateT
import cats.data.State
import cats.syntax.traverse.*
import org.slips.Env
import org.slips.Environment
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.*
import org.slips.core.conditions.Parser.Context
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.network.NetworkLayer
import org.slips.core.rule.Rule

import scala.annotation.tailrec
import scala.collection.SeqView.Sorted
import scala.collection.immutable.SortedSet

object Builder {
  import BuildStep.*

  val materializeAlphaNetwork: BuildStep[Unit] = BuildStep { ctx => ctx -> () }

  val buildAlphaNetwork: Env[BuildStep[NetworkLayer]] = BuildStep { ctx =>
    val network = ctx.network.add(NetworkLayer(ctx.allPredicates))
    ctx.copy(network = network) -> network
  }

  def parse(using env: Environment)(rules: env.Rule*): BuildStep[List[ParseResult[env.Effect]]] = {
    rules.toList.traverse { rule =>
      val x: SelectedPredicatesAndSources = selectPredicatesAndSources(rule.condition)(using rule.T)
      ParseResult(rule, x.facts.map(_.signature.compute), x.predicates)
    }
  }

  def selectPredicatesAndSources[T: FactOps](condition: Condition[T]): Env[SelectedPredicatesAndSources] = {
    val (Context(predicates, _), result) = Parser(condition)

    PredicateSelection.select[T](
      result,
      predicates.flatMap(p => p.facts.map(_ -> p)).groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    )
  }
}
