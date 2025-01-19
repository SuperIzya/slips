package org.slips.core.fact

import org.slips.Signature
import org.slips.Signature.SignatureTuple
import org.slips.core.Empty
import org.slips.core.WithSignature
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.Val

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

  given typeFromTuple: [T <: NonEmptyTuple] => (t: TupleOps[T]) => FactOps[T] = t

  sealed trait TupleOps[T <: NonEmptyTuple] extends FactOps[T] {

    def size: Int = index
    def index: Int
    def indexes(f: Fact.Val[T], size: Int): Map[Fact[?], Int]
  }

  object TupleOps {

    given genTupleOpsStart: [H : { FactOps as H, ScalarFact }]
      => (tupleSig: Signature.SignType.TupleSignature[H *: EmptyTuple])
      => (tuple: Fact.Val[H *: EmptyTuple] =:= Fact[H] *: EmptyTuple) => TupleOps[H *: EmptyTuple] {
      def signature: Signature = tupleSig.signature

      def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      def sources(f: Fact.Val[H *: EmptyTuple]): Set[Fact.Source[?]] = Set(tuple(f).head.source)

      def extract(r: Fact.Val[H *: EmptyTuple]): SignatureTuple =
        SignatureTuple(tuple(r).head.signature)

      def sourceConditions(f: Fact.Val[H *: EmptyTuple]): Set[Condition.Source[?]] =
        tuple(f).head.source.sourceCondition.toList.toSet

      def facts(f: Fact.Val[H *: EmptyTuple]): List[Fact[?]] =
        List(tuple(f).head)

      def indexes(f: Fact.Val[H *: EmptyTuple], size: Int): Map[Fact[?], Int] = Map(f.head -> (size - index))

      def index: Int = 1
    }

    given genTupleOpsStep: [H : { FactOps as H, ScalarFact }, T <: NonEmptyTuple : { TupleOps as T }]
      => (evT: Fact.Val[T] =:= Fact.TMap[T]) => (evF: Fact.Val[H *: T] =:= Fact.TMap[H *: T])
      => (tupleSig: Signature.SignType.TupleSignature[H *: T]) => TupleOps[H *: T] {
      def signature: Signature = tupleSig.signature

      def empty: H *: T = H.empty *: T.empty

      def sources(f: Fact.Val[H *: T]): Set[Fact.Source[?]] =
        combine(f)(_.source, T.sources)(_ + _)

      def extract(f: Fact.Val[H *: T]): SignatureTuple =
        combine(f)(_.signature, T.extract)((t, h) => h +: t)

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

      def sourceConditions(f: Fact.Val[H *: T]): Set[Condition.Source[?]] =
        combine(f)(_.source.sourceCondition, T.sourceConditions)(_ ++ _)

      def indexes(f: Fact.Val[H *: T], size: Int): Map[Fact[?], Int] =
        combine(f)(_ -> (size - index), T.indexes(_, size))(_ + _)

      def index: Int = T.index + 1

      def facts(f: Fact.Val[H *: T]): List[Fact[?]] = combine(f)(identity, T.facts)((t, h) => h +: t)

    }
  }

  given genFactOpsSingle: [T : { ScalarFact as ev, Empty as T, Signature.SignType.TypeSignature as sign }] => FactOps[T] {

    def signature: Signature = sign.signature

    def size: Int = 1

    def empty: T = T.empty

    def sources(f: Fact.Val[T]): Set[Fact.Source[?]] = Set(ev(f).source)

    def extract(r: Fact.Val[T]): SignatureTuple                    = SignatureTuple(ev(r).signature)
    def sourceConditions(f: Fact.Val[T]): Set[Condition.Source[?]] = Set(ev(f).source.sourceCondition).flatten

    def facts(f: Val[T]): List[Fact[?]] = List(ev(f))
  }

}
