package org.slips.data

import zio.test.Gen

sealed trait Data(val cls: String)

object Data {
  case class Fruit(color: Color, sugar: Double, juice: Double, name: String) extends Data("Fruit")

  object Fruit {
    val names: DGen[String] = Gen.elements("apple", "grapes", "orange", "plum")

    given gen(using C: DGen[Color]): DGen[Fruit] = for {
      color <- C
      sugar <- Gen.double
      juice <- Gen.double
      name  <- names
    } yield Fruit(color, sugar, juice, name)
  }

  case class Vegetable(name: String, color: Color) extends Data("Vegetable")

  object Vegetable {
    val names: DGen[String] = Gen.elements("potato", "tomato", "onion", "cabbage")

    given gen(using C: DGen[Color]): DGen[Vegetable] = for {
      color <- C
      name  <- names
    } yield Vegetable(name, color)
  }

  case class Leaf(name: String, size: Double) extends Data("Leaf")

  object Leaf {
    val names: DGen[String] = Gen.elements("lettuce", "coriander", "salad", "spinach")

    given gen(using C: DGen[Color]): DGen[Leaf] = for {
      name <- names
      size <- Gen.double
    } yield Leaf(name, size)
  }

  case class Berry(name: String, color: Color) extends Data("Berry")

  object Berry {
    val names: DGen[String] = Gen.elements("blueberry", "blackberry", "cranberry", "strawberry")

    given gen(using C: DGen[Color]): DGen[Berry] = for {
      color <- C
      name  <- names
    } yield Berry(name, color)
  }

  case class Mushroom(name: String, latin: String) extends Data("Mushroom")

  object Mushroom {
    val names: DGen[String] = Gen.elements("porcini", "truffles", "chanterelle", "armillaria mellea")

    given gen(using C: DGen[Color]): DGen[Mushroom] = for {
      name <- names
    } yield Mushroom(name, name)
  }

  def next: DGen[Data] = Gen.oneOf(Mushroom.gen, Berry.gen, Vegetable.gen, Fruit.gen)
}
