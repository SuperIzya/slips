package org.slips.core.build

import cats.data.IndexedStateT
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
import org.slips.core.network.AlphaNetwork
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule.RuleM
import scala.annotation.tailrec

object Builder {

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

  def parse(using env: Environment)(rules: RuleM*): BuildStep[Unit] = {
    rules.toList.traverse(parseOne(_)).map(_ => ())
  }

  private def parseOne(using env: Environment)(rule: RuleM): BuildStep[ParseResult] = {
    val ps @ SelectedPredicatesAndSources(predicates: PredicateSelectionMap, sources: Set[Source[_]], _, _) = rule
      .sourcesAndPredicates

    case class Partial(alpha: PredicateMap, beta: PredicateMap, gamma: PredicateMap)
    object Partial {
      val empty: Partial = Partial(Map.empty, Map.empty, Map.empty)

      extension (m: PredicateMap)
        def getM(p: Predicate): Set[Fact.Source[_]] = m.getOrElse(p, Set.empty) ++ p.sourceFacts

      extension (pt: Partial) {
        def addPredicate(p: Predicate): Partial = {
          if (p.sourceFacts.size == 1)
            pt.copy(alpha = pt.alpha + (p -> pt.alpha.getM(p)))
          else if (p.sourceFacts.size == 2)
            pt.copy(beta = pt.beta + (p -> pt.beta.getM(p)))
          else pt.copy(gamma = pt.gamma + (p -> pt.gamma.getM(p)))
        }
      }

    }

    val Partial(alpha, beta, gamma) = predicates.values.flatten.foldLeft(Partial.empty)(_ addPredicate _)

    BuildStep.addParsingResult(
      ParseResult(
        rule = rule,
        alphaPredicates = alpha,
        betaPredicates = beta,
        gammaPredicates = gamma,
        sources = sources,
        predicatesAndSources = ps
      )
    )
  }

  def selectPredicatesAndSources[T](condition: Condition[T])(using T: TypeOps[T]): Env[SelectedPredicatesAndSources] = {
    val (Parser.Context(predicates, _), result) = Parser(condition)

    PredicateSelection.select(
      result,
      predicates.flatMap(f => f.sourceFacts.map(_ -> f)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    )
  }
}
