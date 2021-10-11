package org.slips.zio

import zio.Has
import zio._

object Test {

  type T1 = Has[Int]

  type T2 = Has[String]

  type Q = T1 with T2

  val zzz: URIO[Q, Unit] = UIO.unit
}
