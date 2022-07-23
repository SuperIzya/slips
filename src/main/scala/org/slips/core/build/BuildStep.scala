package org.slips.core.build

import cats.data.State
import org.slips.core.fact.Fact

object BuildStep {
  def getOrCreateNode[T](signature: String, fact: Fact.Val[T], node: => BuildStep[Node[T]]): BuildStep[Node[T]] =
    for {
      nodeM <- getNode(signature, fact)
      res   <- nodeM.map(pure) getOrElse node.flatMap(saveNode(signature, fact, _))
    } yield res

  def getNode[T](signature: String, fact: Fact.Val[T]): BuildStep[Option[Node[T]]] =
    apply(_.getNode(signature, fact))

  def saveNode[T](signature: String, fact: Fact.Val[T], node: Node[T]): BuildStep[Node[T]] =
    apply(_.addNode(signature, fact, node))

  def apply[T](f: BuildContext => (BuildContext, T)): BuildStep[T] = State(f)

  def pure[T](t: T): BuildStep[T] = State.pure(t)

}
