package org.slips.core

import cats.Applicative
import cats.compat.SortedSet
import cats.data.State
import cats.implicits.*
import org.slips.Env
import org.slips.core.conditions.Predicate
import org.slips.core.fact.Fact
import org.slips.core.rule.Rule.RuleM

package object build {

  type AlphaFacts      = Map[Fact.Source, Set[Predicate]]
  type AllFacts        = Map[Fact[?], Set[Predicate]]
  type AlphaPredicates = Map[String, AlphaPredicate]
  type BetaPredicates  = Map[Predicate, Set[Fact[?]]]
  type PredicateRules  = Map[Predicate, Set[RuleM]]

  type BuildStep[x] = State[BuildContext, ? <: x]

  extension (a: AlphaPredicates) {
    def addAlpha(predicate: Predicate): Env[AlphaPredicates] = {
      val alphaPredicate = AlphaPredicate(predicate)
      alphaPredicate.fold(a)(p => a |+| Map(p.source -> p))
    }
  }

  extension (b: BetaPredicates) {
    def addBeta(predicate: Predicate): BetaPredicates = {
      b + (predicate -> (b.getOrElse(predicate, Set.empty) ++ predicate.facts))
    }
  }

  given Applicative[BuildStep] = new Applicative[BuildStep] {
    override def pure[A](x: A): BuildStep[A] = BuildStep.pure(x)

    override def ap[A, B](ff: BuildStep[A => B])(fa: BuildStep[A]): BuildStep[B] = for {
      a <- fa
      f <- ff
    } yield f(a)
  }

}
