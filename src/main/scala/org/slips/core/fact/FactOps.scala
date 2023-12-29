package org.slips.core.fact

import org.slips.Signature
import org.slips.Signature.SignatureTuple
import org.slips.core.Empty
import org.slips.core.Macros
import org.slips.core.Signed
import org.slips.core.conditions.Condition

sealed trait FactOps[T] extends Signed {
  override val signature: Signature = Signature.Manual(Macros.signType[T])

  def size: Int

  def empty: T

  def extract(r: Fact.Val[T]): SignatureTuple
  def predecessors(f: Fact.Val[T]): Set[Fact[_]]
  def sources(f: Fact.Val[T]): Set[Condition.Source[_]]
  def sourceFacts(f: Fact.Val[T]): Set[Fact.Source[_]]
}

object FactOps {

  def apply[T](using T: FactOps[T]): FactOps[T] = T

  given typeFromTuple[T <: NonEmptyTuple](using T: TupleOps[T]): FactOps[T] = T

  sealed trait TupleOps[T <: NonEmptyTuple]
      extends FactOps[T] {

    override lazy val size: Int = index
    val index: Int
  }

  object TupleOps {
    given genTupleOpsStart[H](
      using H: FactOps[H],
      ev: ScalarFact[H],
      tuple: Fact.Val[H *: EmptyTuple] =:= Fact[H] *: EmptyTuple
    ): TupleOps[H *: EmptyTuple] with {
      override val index: Int = 1

      override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      override def sourceFacts(f: Fact.Val[H *: EmptyTuple]): Set[Fact.Source[_]] = tuple(f).head.sourceFacts

      override def extract(r: Fact.Val[H *: EmptyTuple]): SignatureTuple =
        SignatureTuple.first(tuple(r).head.signature)

      def predecessors(f: Fact.Val[H *: EmptyTuple]): Set[Fact[_]] =
        tuple(f).head.predecessors

      def sources(f: Fact.Val[H *: EmptyTuple]): Set[Condition.Source[_]] =
        tuple(f).head.sources
    }

    given genTupleOpsStep[H, T <: NonEmptyTuple](
      using H: FactOps[H],
      evH: ScalarFact[H],
      T: TupleOps[T],
      evT: Fact.Val[T] =:= Fact.TMap[T],
      evF: Fact.Val[H *: T] =:= Fact.TMap[H *: T]
    ): TupleOps[H *: T] with {
      override val index: Int = T.index + 1

      override def empty: H *: T = H.empty *: T.empty

      override def sourceFacts(f: Fact.Val[H *: T]): Set[Fact.Source[_]] =
        combine(f)(_.sourceFacts, T.sourceFacts)(_ ++ _)

      override def extract(f: Fact.Val[H *: T]): SignatureTuple =
        combine(f)(_.signature, T.extract)((h, t) => t.prepend(h))

      override def predecessors(f: Fact.Val[H *: T]): Set[Fact[_]] =
        combine(f)(_.predecessors, T.predecessors)(_ ++ _)

      override def sources(f: Fact.Val[H *: T]): Set[Condition.Source[_]] =
        combine(f)(_.sources, T.sources(_))(_ ++ _)

      private def combine[A, B, C](
        f: Fact.TMap[H *: T]
      )(
        head: Fact[H] => A,
        tail: Fact.Val[T] => B
      )(
        combine: (A, B) => C
      ): C =
        combine(
          head(f.head),
          tail(evT.flip(f.tail))
        )
    }
  }

  given genFactOpsSingle[T](using ev: ScalarFact[T], T: Empty[T]): FactOps[T] with {

    override def size: Int = 1

    override def empty: T = T.empty

    override def sourceFacts(f: Fact.Val[T]): Set[Fact.Source[_]] = ev(f).sourceFacts

    override def extract(r: Fact.Val[T]): SignatureTuple  = SignatureTuple.first(ev(r).signature)
    def predecessors(f: Fact.Val[T]): Set[Fact[_]]        = ev(f).predecessors
    def sources(f: Fact.Val[T]): Set[Condition.Source[_]] = ev(f).sources
  }

}
