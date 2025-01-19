package org.slips

import cats.Eq
import cats.Order
import org.slips.core.Empty
import scala.annotation.targetName
import zio.test.Gen

package object data {
  type DGen[x] = Gen[Any, x]

  enum Color   {
    case Green, Yellow, Red, Brown
  }
  object Color {
    given gen: DGen[Color] = Gen.elements(Color.Green, Color.Yellow, Color.Red, Color.Brown)

    given Order[Color] = Order.from((a, b) => a.ordinal.compareTo(b.ordinal))
  }

  extension (a: Color) {
    @targetName("neq")
    def =:!:=(b: Color): Boolean = a.ordinal != b.ordinal
    @targetName("eq")
    def =:=(b: Color): Boolean   = a.ordinal == b.ordinal
  }

  extension (a: Origin) {
    @targetName("neq")
    def !==(b: Origin): Boolean = a.ordinal != b.ordinal
    @targetName("eq")
    def ===(b: Origin): Boolean = a.ordinal == b.ordinal
  }

  object Origin {
    given Empty[Origin] {
      override def empty: Origin = Origin.GreenHouse
    }

    given Eq[Origin] = (x: Origin, y: Origin) => x === y
  }

  enum Origin {
    case Field, GreenHouse
  }

}
