package org.slips.core.build

import cats.data.State
import org.slips.Env
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.network.Node
import org.slips.core.network.alpha.AlphaNode

object BuildStep {

  def apply[T](f: BuildContext => (BuildContext, T)): BuildStep[T] = State(f)

  def update(f: BuildContext => BuildContext): BuildStep[Unit] = State(f.andThen(_ -> ()))
  def set(f: => BuildContext): BuildStep[Unit]                 = State.set(f)
  /*
  def getSourceNode[T](src: Condition.Source[T]): Env[BuildStep[AlphaNode.Source[T]]] = env ?=> {
    val signature = env.signatureStrategy(src.signature)
    BuildStep(_.addSourceNode(signature, AlphaNode.Source(src.signature)))
  }*/

  val get: BuildStep[BuildContext] = State.get[BuildContext]

  def addParsingResult(p: ParseResult): BuildStep[ParseResult] =
    State(s => s.addParsingResult(p) -> p)

  def pure[T](t: T): BuildStep[T] = State.pure(t)

}
