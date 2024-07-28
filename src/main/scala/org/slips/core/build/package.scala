package org.slips.core

import cats.Applicative
import cats.compat.SortedSet
import cats.data.State
import cats.implicits.*
import org.slips.{Env, Environment}
import org.slips.core.conditions.Predicate
import org.slips.core.fact.Fact
import org.slips.core.network.{AlphaNode, NetworkLayer, Node}
import org.slips.core.rule.Rule

package object build {
  private[slips] type AllFacts        = Map[Fact.Source[?], Set[Predicate]]
  private[slips] type AllPredicates = Map[String, BuildPredicate]
  private[slips] type PredicateRules[F[_]]  = Map[Predicate, Set[Rule[F]]]

  private[slips] type BuildStep[F[_]] = [x] =>> State[BuildContext[F], ? <: x]
  private[slips] type BuildStepF[F[_]] = [g[_[_]]] =>> State[BuildContext[F], g[F]]
  private[slips] type EnvBuildStep[x] = (env: Environment) ?=> BuildStep[env.Effect][x]
  private[slips] type EnvBuildStepF[g[_[_]]] = (env: Environment) ?=> BuildStepF[env.Effect][g]

  private[slips] type EnvBuildContext = (env: Environment) ?=> BuildContext[env.Effect]
  private[slips] type EnvNetworkLayer = (env: Environment) ?=> NetworkLayer[env.Effect]
  private[slips] type EnvParseResult = (env: Environment) ?=> ParseResult[env.Effect]
  private[slips] type EnvBuildParseResult = (env: Environment) ?=> BuildStep[env.Effect][ParseResult[env.Effect]]

  private[slips] given app[F[_]]: Applicative[BuildStep[F]] = new Applicative[BuildStep[F]] {
    override def pure[A](x: A): BuildStep[F][A] = BuildStep.pure(x)

    override def ap[A, B](ff: BuildStep[F][A => B])(fa: BuildStep[F][A]): BuildStep[F][B] = for {
      a <- fa
      f <- ff
    } yield f(a)
  }

  extension (b: BuildStep.type) {
    private[slips] def addNode(using env: Environment)(n: Node[env.Effect]): BuildStepF[env.Effect][Node] =
      n match {
        case a: AlphaNode.Source[env.Effect, ?] =>
        for {
          ctx <- b.get
          _ <- ctx.addSourceNode(a.signature, a)
        } yield n
          
        case _                      =>
          for {
            ctx <- b.get
            _ <- ctx.addNode(n)
          } yield n
      }
  }

}
