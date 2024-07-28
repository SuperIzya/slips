package org.slips

import cats.Eq
import org.slips.data.Origin

import scala.annotation.targetName

package object core {

  case class Fruit(name: String, sugar: Double, acidity: Double)

  case class Vegetable(name: String, origin: Origin)

  case class Herb(name: String, origin: Origin)

  case class Berry(name: String, origin: Origin)

}
