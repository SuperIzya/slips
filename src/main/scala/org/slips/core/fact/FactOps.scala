package org.slips.core.fact

import org.slips.Signature
import org.slips.Signature.SignatureTuple
import org.slips.core.Empty
import org.slips.core.WithSignature
import org.slips.core.macros.Macros
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact.Val

sealed trait FactOps[T] extends WithSignature {
  def size: Int

  def empty: T

  def extract(r: Fact.Val[T]): SignatureTuple
  def predecessors(f: Fact.Val[T]): List[Fact[?]]
  def sources(f: Fact.Val[T]): Set[Condition.Source[?]]
  def alphaSources(f: Fact.Val[T]): Set[Fact.Source]
  def facts(f: Fact.Val[T]): List[Fact[?]]
}

object FactOps {

  def apply[T](using T: FactOps[T]): FactOps[T] = T

  given typeFromTuple[T <: NonEmptyTuple](using T: TupleOps[T]): FactOps[T] = T

  sealed trait TupleOps[T <: NonEmptyTuple]
      extends FactOps[T] {

    override def signature: SignatureTuple

    override def size: Int = index
    def index: Int
    def indexes(f: Fact.Val[T], size: Int): Map[Fact[?], Int]
  }

  object TupleOps {
    given genTupleOpsStart[H](using
      H: FactOps[H],
      S: Signature.Typed[H],
      ev: ScalarFact[H],
      tuple: Fact.Val[H *: EmptyTuple] =:= Fact[H] *: EmptyTuple
    ): TupleOps[H *: EmptyTuple] with {
      override def index: Int = 1

      override def signature: SignatureTuple = SignatureTuple(S.toSignature)

      override def empty: H *: EmptyTuple = H.empty *: EmptyTuple

      override def alphaSources(f: Fact.Val[H *: EmptyTuple]): Set[Fact.Source] = tuple(f).head.alphaSources

      override def extract(r: Fact.Val[H *: EmptyTuple]): SignatureTuple =
        SignatureTuple(tuple(r).head.signature)

      def predecessors(f: Fact.Val[H *: EmptyTuple]): List[Fact[?]] =
        tuple(f).head.predecessors

      def sources(f: Fact.Val[H *: EmptyTuple]): Set[Condition.Source[?]] =
        tuple(f).head.sources

      def facts(f: Fact.Val[H *: EmptyTuple]): List[Fact[?]] =
        List(tuple(f).head)

      def indexes(f: Fact.Val[H *: EmptyTuple], size: Int): Map[Fact[?], Int] = Map(f.head -> (size - index))
    }

    given genTupleOpsStep[H, T <: NonEmptyTuple](using
      H: FactOps[H],
                                                 Sh: Signature.Typed[H],
      evH: ScalarFact[H],
      T: TupleOps[T],
      evT: Fact.Val[T] =:= Fact.TMap[T],
      evF: Fact.Val[H *: T] =:= Fact.TMap[H *: T]
    ): TupleOps[H *: T] with {
      override def index: Int = T.index + 1

      override def signature: SignatureTuple = Sh.toSignature +: T.signature

      override def empty: H *: T = H.empty *: T.empty

      override def alphaSources(f: Fact.Val[H *: T]): Set[Fact.Source] =
        combine(f)(_.alphaSources, T.alphaSources)(_ ++ _)

      override def extract(f: Fact.Val[H *: T]): SignatureTuple =
        combine(f)(_.signature, T.extract)((h, t) => h +: t)

      override def predecessors(f: Fact.Val[H *: T]): List[Fact[?]] =
        combine(f)(_.predecessors, T.predecessors)(_ ++ _)

      override def sources(f: Fact.Val[H *: T]): Set[Condition.Source[?]] =
        combine(f)(_.sources, T.sources)(_ ++ _)

      def indexes(f: Fact.Val[H *: T], size: Int): Map[Fact[?], Int] =
        combine(f)(_ -> (size - index), T.indexes(_, size))((a, m) => m + a)

      override def facts(f: Val[H *: T]): List[Fact[?]] = combine(f)(identity, T.facts)(_ +: _)

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

  given genFactOpsSingle[T](using ev: ScalarFact[T], T: Empty[T], S: Signature.Typed[T]): FactOps[T] with {

    override def size: Int = 1

    override def empty: T = T.empty

    override def alphaSources(f: Fact.Val[T]): Set[Fact.Source] = ev(f).alphaSources

    override val signature: Signature = S.toSignature

    override def extract(r: Fact.Val[T]): SignatureTuple  = SignatureTuple(ev(r).signature)
    def predecessors(f: Fact.Val[T]): List[Fact[?]]       = ev(f).predecessors
    def sources(f: Fact.Val[T]): Set[Condition.Source[?]] = ev(f).sources

    override def facts(f: Val[T]): List[Fact[?]] = List(ev(f))
  }

}
