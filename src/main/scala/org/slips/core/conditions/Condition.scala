package org.slips.core.conditions

import org.slips.Signature
import org.slips.core.WithSignature
import org.slips.core.fact.*
import org.slips.core.fact.Fact.Val
import org.slips.core.macros.Macros

sealed trait Condition[T](using val T: FactOps[T])

object Condition {

  inline def all[T : FactOps : ScalarFact]: All[T] =
    All[T](Signature.Manual(s"All[${ Macros.signType[T] }]"))

  sealed trait Source[T: FactOps](using val ev: ScalarFact[T]) extends Condition[T] with WithSignature

  final case class All[T : FactOps : ScalarFact] private[Condition] (override val signature: Signature)
      extends Source[T]

  final case class Opaque[T] private[slips] (predicate: Predicate) extends Condition[Unit]

  final case class Map[T, Q: FactOps] private[slips] (src: Condition[T], f: Fact.Val[T] => Fact.Val[Q])
      extends Condition[Q]

  final case class FlatMap[T, Q: FactOps] private[slips] (left: Condition[T], f: Fact.Val[T] => Condition[Q])
      extends Condition[Q]

  final case class Filter[T: FactOps] private[slips] (
    cond: Condition[T],
    f: Fact.Val[T] => Predicate
  ) extends Condition[T]
}
