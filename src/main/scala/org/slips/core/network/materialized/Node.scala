package org.slips.core.network.materialized

trait Node[F[_]] {
  val publisher: Publisher[F, ?]
}

object Node {
  trait Alpha[F[_]] extends Node[F] {
    val source: Subscriber
  }
  trait Beta[F[_]]  extends Node[F] {
    val left: Subscriber
    val right: Subscriber
  }
  trait Gamma[F[_]] extends Node[F] {
    val source: Subscriber
  }
}
