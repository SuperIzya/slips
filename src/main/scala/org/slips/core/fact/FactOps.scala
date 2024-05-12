package org.slips.core.fact

import org.slips.Signature
import org.slips.Signature.SignatureTuple
import org.slips.core.{Empty, WithSignature}
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.Val
import org.slips.core.macros.Macros

sealed trait FactOps[T] extends WithSignature {
  def size: Int

  def empty: T

  def extract(r: Fact.Val[T]): SignatureTuple
  def sourceConditions(f: Fact.Val[T]): Set[Condition.Source[?]]
  def sources(f: Fact.Val[T]): Set[Fact.Source[?]]
  def facts(f: Fact.Val[T]): List[Fact[?]]
}

object FactOps {

  def apply[T](using T: FactOps[T]): FactOps[T] = T

  given typeFromTuple[T <: NonEmptyTuple](using T: TupleOps[T]): FactOps[T] = T

  sealed trait TupleOps[T <: NonEmptyTuple]
      extends FactOps[T] {

    override def size: Int = index
    def index: Int
    def indexes(f: Fact.Val[T], size: Int): Map[Fact[?], Int]
  }

  object TupleOps {

    given genTupleOpsStart[H](using
      H: FactOps[H],
      ev: ScalarFact[H],
      tupleSig: Signature.SignType.TupleSignature[H *: EmptyTuple],
      tuple: Fact.Val[H *: EmptyTuple] =:= Fact[H] *: EmptyTuple
    ): TupleOps[H *: EmptyTuple] with {
      override def signature: Signature = tupleSig.signature

      override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      override def sources(f: Fact.Val[H *: EmptyTuple]): Set[Fact.Source[?]] = Set(tuple(f).head.source)

      override def extract(r: Fact.Val[H *: EmptyTuple]): SignatureTuple =
        SignatureTuple(tuple(r).head.signature)


      def sourceConditions(f: Fact.Val[H *: EmptyTuple]): Set[Condition.Source[?]] =
        tuple(f).head.source.sourceCondition.toList.toSet

      def facts(f: Fact.Val[H *: EmptyTuple]): List[Fact[?]] =
        List(tuple(f).head)

      def indexes(f: Fact.Val[H *: EmptyTuple], size: Int): Map[Fact[?], Int] = Map(f.head -> (size - index))

      override def index: Int = 1
    }

    given genTupleOpsStep[H, T <: NonEmptyTuple](using
      H: FactOps[H],
      evH: ScalarFact[H],
      T: TupleOps[T],
      evT: Fact.Val[T] =:= Fact.TMap[T],
      evF: Fact.Val[H *: T] =:= Fact.TMap[H *: T],
      tupleSig: Signature.SignType.TupleSignature[H *: T]
    ): TupleOps[H *: T] with {
      override def signature: Signature = tupleSig.signature

      override def empty: H *: T = H.empty *: T.empty

      override def sources(f: Fact.Val[H *: T]): Set[Fact.Source[?]] =
        combine(f)(_.source, T.sources)(_ + _)

      override def extract(f: Fact.Val[H *: T]): SignatureTuple =
        combine(f)(_.signature, T.extract)((t, h) => h +: t)

      override def sourceConditions(f: Fact.Val[H *: T]): Set[Condition.Source[?]] =
        combine(f)(_.source.sourceCondition, T.sourceConditions)(_ ++ _)

      def indexes(f: Fact.Val[H *: T], size: Int): Map[Fact[?], Int] =
        combine(f)(_ -> (size - index), T.indexes(_, size))(_ + _)

      private inline def combine[A, B, C](
        f: Fact.TMap[H *: T]
      )(
        inline head: Fact[H] => A,
        inline tail: Fact.Val[T] => B
      )(
        inline combine: (B, A) => C
      ): C =
        combine(
          tail(evT.flip(f.tail)),
          head(f.head)
        )

      override def index: Int = T.index + 1

      override def facts(f: Fact.Val[H *: T]): List[Fact[?]] = combine(f)(identity, T.facts)((t, h) => h +: t)

    }
  }

  given genFactOpsSingle[T](using ev: ScalarFact[T], T: Empty[T], sign: Signature.SignType.TypeSignature[T]): FactOps[T]
  with {

    override def signature: Signature = sign.signature

    override def size: Int = 1

    override def empty: T = T.empty

    override def sources(f: Fact.Val[T]): Set[Fact.Source[?]] = Set(ev(f).source)

    override def extract(r: Fact.Val[T]): SignatureTuple  = SignatureTuple(ev(r).signature)
    def sourceConditions(f: Fact.Val[T]): Set[Condition.Source[?]] = Set(ev(f).source.sourceCondition).flatten

    override def facts(f: Val[T]): List[Fact[?]] = List(ev(f))
  }

}
