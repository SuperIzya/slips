package org.slips.core.build

import org.slips.core.fact.Fact

case class BuildContext private[build] (nodes: Map[String, Node[_]]) {
  def getNode[T](signature: String, fact: Fact.Val[T]): (BuildContext, Option[Node[T]])        = this -> None
  def addNode[T](signature: String, fact: Fact.Val[T], node: Node[T]): (BuildContext, Node[T]) =
    this.copy(nodes = nodes + (signature -> node)) -> node
}
