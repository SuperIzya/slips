package org.slips

import cats.Eq
import zio.test.Gen

package object data {
  type DGen[x] = Gen[Any, x]

  enum Color:
    case Green, Yellow, Red, Brown

  object Color {
    given gen: DGen[Color] = Gen.elements(Color.Green, Color.Yellow, Color.Red, Color.Brown)

    given Eq[Color] = (a: Color, b: Color) => a == b
  }
}
