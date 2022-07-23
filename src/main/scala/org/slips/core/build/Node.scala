package org.slips.core.build

import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate

sealed trait Node[T] {
  def signature: String
  def intake: Node.Intake[_]
  val sink: Node.Sink[T] = Node.Sink()
  val fact: Fact.Val[T]
}

object Node {

  type NonEmptyConcat[X, Y <: NonEmptyTuple] <: NonEmptyTuple = X match
    case x *: t     => x *: Tuple.Concat[t, Y]
    case EmptyTuple => Y
    case _          => X *: Y

  type Combine[X, Y] <: NonEmptyTuple = Y match
    case NonEmptyTuple => NonEmptyConcat[X, Y]
    case EmptyTuple    =>
      X match
        case h *: t     => h *: t
        case EmptyTuple => Tuple1[Unit]
        case _          => Tuple1[X]
    case _             =>
      X match
        case h *: t     => h *: Tuple.Append[t, Y]
        case EmptyTuple => Tuple1[Y]
        case _          => (X, Y)

  sealed trait PNode[T] {
    def predicate: Predicate
  }

  case class All[T] private (fact: Fact.Val[T], signature: String) extends Node[T] {
    override val intake: Intake[_] = EmptyIntake
  }

  object All {
    def apply[T](signature: String, fact: Fact.Val[T]): BuildStep[Node[T]] =
      BuildStep.pure(new All(fact, signature))
  }

  case class Sink[T]()

  sealed trait Intake[T]
  case object EmptyIntake                      extends Intake[Any]
  final case class Intake1[T](source: Sink[T]) extends Intake[T]
  final case class Intake2[T1, T2](source1: Sink[T1], source2: Sink[T2])
      extends Intake[T1 *: T2 *: EmptyTuple]

}
