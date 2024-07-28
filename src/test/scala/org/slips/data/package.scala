package org.slips

import cats.Eq
import org.slips.core.Empty
import zio.test.Gen

import scala.annotation.targetName

package object data {
  type DGen[x] = Gen[Any, x]

  enum Color {
    case Green, Yellow, Red, Brown
  }
  object Color {
    given gen: DGen[Color] = Gen.elements(Color.Green, Color.Yellow, Color.Red, Color.Brown)

    given Eq[Color] = (a: Color, b: Color) => a === b
  }


  extension (a: Color) {
    @targetName("neq")
    def !==(b: Color): Boolean = a.ordinal != b.ordinal
    @targetName("eq")
    def ===(b: Color): Boolean = a.ordinal == b.ordinal
  }

  extension (a: Origin) {
    @targetName("neq")
    def !==(b: Origin): Boolean = a.ordinal != b.ordinal
    @targetName("eq")
    def ===(b: Origin): Boolean = a.ordinal == b.ordinal
  }

  object Origin {
    given Empty[Origin] with {
      override def empty: Origin = Origin.GreenHouse
    }

    given Eq[Origin] = (x: Origin, y: Origin) => x === y
  }

  enum Origin {
    case Field, GreenHouse
  }

}
