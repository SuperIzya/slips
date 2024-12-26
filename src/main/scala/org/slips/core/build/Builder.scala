package org.slips.core.build

import cats.data.IndexedStateT
import org.slips.Env
import org.slips.Environment
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.*
import org.slips.core.conditions.Parser.Context
import org.slips.core.fact.FactOps
import org.slips.core.network.NetworkLayer
import org.slips.core.rule.Rule

private[slips] object Builder {
  import BuildStep.*

  val materializeAlphaNetwork: EnvBuildStep[Unit] = BuildStep { ctx => ctx -> () }

  val buildNetwork: EnvBuildStepF[NetworkLayer] = (env: Environment) ?=> {
    for {
      ctx <- BuildStep.get
      network: NetworkLayer[env.Effect] = NetworkLayer(ctx.allPredicates)
      _ <- ctx.addNetwork(network)
    } yield network
  }

  def parse(using env: Environment)(rules: env.Rule*): Result[BuildStep[env.Effect][Unit]] = {
    rules
      .toList
      .map { rule =>
        selectPredicatesAndSources(rule.condition)(using rule.T).map { x =>
          ParseResult(
            rule = rule,
            sources = x.facts.map(_.signature.compute),
            allPredicates = x
              .predicates
              .map(p => p.signature.compute -> p)
              .groupBy(_._1)
              .view
              .mapValues { v =>
                val head  = v.head
                val tail  = v.tail
                val facts = head._2.facts
                tail.foldLeft(BuildPredicate(facts, facts.size, head._2)) { case (bp, (_, p)) =>
                  bp.copy(facts = bp.facts ++ p.facts)
                }
              }
              .toMap,
            predicatesAndSources = x
          )
        }
      }
      .foldLeft(Result.result(BuildStep.empty(env))) { (collected, element) =>
        val res: Result[BuildStep[env.Effect][Unit]] = for {
          el  <- element
          col <- collected
        } yield {
          for {
            _ <- col
            _ <- BuildStep.addParsingResult(el)
          } yield ()
        }
        res
      }
  }

  def selectPredicatesAndSources[T: FactOps](condition: Condition[T]): Env[Result[SelectedPredicatesAndSources]] = {
    val (Context(predicates, _), result) = Parser(condition)

    PredicateSelection.select[T](
      result,
      predicates.flatMap(p => p.facts.map(_ -> p)).groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    )
  }
}
