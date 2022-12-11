package org.slips.generator

import org.slips.data.Data
import org.slips.data.Data.*
import org.slips.syntax.*
import zio.test.Gen

trait PossibleAlpha[D <: Data] {
  def next(d: D): AlphaGen[D]
}

object PossibleAlpha {
  private def cls[D <: Data](t: D): Alpha[D] = Alpha(_.value(_.cls) === t.cls)

  given paFruit: PossibleAlpha[Fruit] = (d: Fruit) =>
    Gen.elements[Alpha[Fruit]](
      _.value(_.color) === d.color,
      _.value(_.name) === d.name,
      _.value(_.sugar) === d.sugar,
      _.value(_.juice) === d.juice,
      cls(d)
    )

  given paVegie: PossibleAlpha[Vegetable] = (v: Vegetable) =>
    Gen.elements[Alpha[Vegetable]](
      _.value(_.name) === v.name,
      _.value(_.color) === v.color,
      cls(v)
    )

  given paLeaf: PossibleAlpha[Leaf] = (l: Leaf) =>
    Gen.elements[Alpha[Leaf]](
      _.value(_.name) === l.name,
      _.value(_.size) === l.size,
      cls(l)
    )

  given paBerry: PossibleAlpha[Berry] = (b: Berry) =>
    Gen.elements[Alpha[Berry]](
      _.value(_.name) === b.name,
      _.value(_.color) === b.color,
      cls(b)
    )

  given paShroom: PossibleAlpha[Mushroom] = (m: Mushroom) =>
    Gen.elements[Alpha[Mushroom]](
      _.value(_.name) === m.name,
      _.value(_.latin) === m.latin,
      cls(m)
    )
}
