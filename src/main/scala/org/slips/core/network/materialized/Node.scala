package org.slips.core.network.materialized

trait Node {
  val publisher: Publisher[?]
}

object Node {
  trait Alpha extends Node {
    val source: Subscriber
  }
  trait Beta  extends Node {
    val left: Subscriber
    val right: Subscriber
  }
  trait Gamma extends Node {
    val source: Subscriber
  }
}
