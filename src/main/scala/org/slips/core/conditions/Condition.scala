package org.slips.core.conditions

import org.slips.Signature
import org.slips.core.SourceLocation
import org.slips.core.WithSignature
import org.slips.core.fact.*
import org.slips.core.fact.Fact.Val
import org.slips.core.macros.Macros

sealed trait Condition[T](using val T: FactOps[T])

object Condition {

  sealed trait Source[T: FactOps](using val ev: ScalarFact[T], val sourceLocation: SourceLocation) extends Condition[T]
      with WithSignature

  final case class All[T : {FactOps, ScalarFact}] private[slips] (override val signature: Signature)(
    using SourceLocation
  ) extends Source[T]

  final case class Opaque[T] private[slips] (predicate: Predicate)(using val sourceLocation: SourceLocation)
      extends Condition[Unit]

  final case class Map[T, Q: FactOps] private[slips] (src: Condition[T], f: Fact.Val[T] => Fact.Val[Q])(
    using val sourceLocation: SourceLocation
  ) extends Condition[Q]

  final case class FlatMap[T, Q: FactOps] private[slips] (left: Condition[T], f: Fact.Val[T] => Condition[Q])(
    using val sourceLocation: SourceLocation
  ) extends Condition[Q]

  final case class Filter[T: FactOps] private[slips] (
    cond: Condition[T],
    f: Fact.Val[T] => Predicate
  )(using val sourceLocation: SourceLocation) extends Condition[T]
}
