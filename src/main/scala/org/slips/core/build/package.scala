package org.slips.core

import cats.Applicative
import cats.compat.SortedSet
import cats.data.State
import cats.implicits.*
import org.slips.{Env, Environment}
import org.slips.core.conditions.Predicate
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.network.AlphaNode
import org.slips.core.rule.Rule

package object build {

  type AllFacts        = Map[Fact.Source[?], Set[Predicate]]
  type AllPredicates = Map[String, BuildPredicate]
  type PredicateRules[F[_]]  = Map[Predicate, Set[Rule[F]]]

  type BuildStep[x] = (env: Environment) ?=> State[BuildContext[env.Effect], ? <: x]


  given Applicative[BuildStep] = new Applicative[BuildStep] {
    override def pure[A](x: A): BuildStep[A] = BuildStep.pure(x)

    override def ap[A, B](ff: BuildStep[A => B])(fa: BuildStep[A]): BuildStep[B] = for {
      a <- fa
      f <- ff
    } yield f(a)
  }

  extension (b: BuildStep.type) {
    private[slips] def addNode[N <: Node](n: N): BuildStep[Node] = n match {
      case a: AlphaNode.Source[?] => b(_.addSourceNode(a.signature, a))
      case _                      => b(_.addNode(n))
    }
  }

}
