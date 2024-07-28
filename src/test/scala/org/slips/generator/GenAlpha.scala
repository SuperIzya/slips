package org.slips.generator

import org.slips.data.Data
import org.slips.data.Data.*
import org.slips.data.*
import org.slips.syntax.*
import zio.test.Gen

trait GenAlpha[D <: Data] {
  def next(d: D): AlphaGen[D]
}

object GenAlpha {
  private def cls[D <: Data](t: D): Alpha[D] = Alpha(_.test(_.cls == t.cls))

  given paFruit: GenAlpha[Fruit] = (d: Fruit) =>
    Gen.elements[Alpha[Fruit]](
      _.test(_.color === d.color),
      _.test(_.name == d.name),
      _.test(_.sugar == d.sugar),
      _.test(_.juice == d.juice),
      cls(d)
    )

  given paVegie: GenAlpha[Vegetable] = (v: Vegetable) =>
    Gen.elements[Alpha[Vegetable]](
      _.test(_.name == v.name),
      _.test(_.color === v.color),
      cls(v)
    )

  given paLeaf: GenAlpha[Leaf] = (l: Leaf) =>
    Gen.elements[Alpha[Leaf]](
      _.test(_.name == l.name),
      _.test(_.size == l.size),
      cls(l)
    )

  given paBerry: GenAlpha[Berry] = (b: Berry) =>
    Gen.elements[Alpha[Berry]](
      _.test(_.name == b.name),
      _.test(_.color === b.color),
      cls(b)
    )

  given paShroom: GenAlpha[Mushroom] = (m: Mushroom) =>
    Gen.elements[Alpha[Mushroom]](
      _.test(_.name == m.name),
      _.test(_.latin == m.latin),
      cls(m)
    )
}
