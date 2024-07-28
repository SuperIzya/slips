package org.slips.core.build

import cats.data.State
import org.slips.{Env, Environment}
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.network.{AlphaNode, Node}

private[slips] object BuildStep {

  def get: EnvBuildStepF[BuildContext] = env ?=> State.get[BuildContext[env.Effect]]

  def pure[F[_], T](pair: (BuildContext[F], T)): BuildStep[F][T] = State.set(pair._1).map(_ => pair._2)

  def apply[T](f: (env: Environment) ?=> BuildContext[env.Effect] => (BuildContext[env.Effect], T)): EnvBuildStep[T] =
    env ?=> State(f)

  def pureF[F[_], G[_[_]]](pair: (BuildContext[F], G[F])): BuildStepF[F][G] = State.set(pair._1).map(_ => pair._2)

  def F[G[_[_]]](f: (env: Environment) ?=> BuildContext[env.Effect] => (BuildContext[env.Effect], G[env.Effect])): EnvBuildStepF[G] =
    env ?=> State(f)

  def update(f: (env: Environment) ?=> BuildContext[env.Effect] => BuildContext[env.Effect]): EnvBuildStep[Unit] =
    env ?=> State(f.andThen(_ -> ()))
  /*
  def getSourceNode[T](src: Condition.Source[T]): Env[BuildStep[AlphaNode.Source[T]]] = env ?=> {
    val signature = env.signatureStrategy(src.signature)
    BuildStep(_.addSourceNode(signature, AlphaNode.Source(src.signature)))
  }*/

  def set(f: EnvBuildContext): EnvBuildStep[Unit] = State.set(f)

  def addParsingResult(using env: Environment)(p: ParseResult[env.Effect]): BuildStep[env.Effect][ParseResult[env.Effect]] =
    State(_.addParsingResult(p) -> p)

  def pure[F[_], T](t: T): BuildStep[F][T] = State.pure(t)

  def empty(env: Environment): BuildStep[env.Effect][Unit] = State.pure(())
}
