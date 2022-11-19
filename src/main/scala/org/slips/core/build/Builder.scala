package org.slips.core.build

import cats.data.IndexedStateT
import cats.data.State
import cats.syntax.traverse.*
import org.slips.Env
import org.slips.Environment
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.Condition
import org.slips.core.conditions.Condition.Source
import org.slips.core.conditions.Parser
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.network.AlphaNetwork
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM
import scala.annotation.tailrec

object Builder {
  import BuildStep.*

  val materializeAlphaNetwork: BuildStep[Unit] = BuildStep { ctx => ctx -> () }

  val buildAlphaNetwork: Env[BuildStep[AlphaNetwork]] = env ?=>
    BuildStep { ctx =>
      val network = ctx
        .alphaPredicates
        .toList
        .sortBy(-_._2.size)
        .foldLeft(AlphaNetwork(ctx.sources))(env.alphaNodeStrategy.fold)
        .toAlphaNetwork

      ctx.addAlphaNetwork(network) -> network
    }

  def parse(using env: Environment)(rules: RuleM*): BuildStep[List[ParseResult]] = {
    rules.toList.traverse { r => BuildStep.addParsingResult(ParseResult.fromRule(r)) }
  }

  def selectPredicatesAndSources[T](condition: Condition[T])(using T: FactOps[T]): Env[SelectedPredicatesAndSources] = {
    val (Parser.Context(predicates, _), result) = Parser(condition)

    PredicateSelection.select(
      result,
      predicates.flatMap(f => f.sourceFacts.map(_ -> f)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    )
  }
}
