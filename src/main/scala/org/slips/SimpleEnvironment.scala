package org.slips

import cats.Id
import cats.Monad
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.fact.Fact
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait SimpleEnvironment extends Environment {
  override type Effect[x] = cats.Id[x]
  override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean

  override given effectMonad: Monad[Id] = new Monad[Id] {
    override def pure[T](
      v: T
    ): Id[T] = v

    override def flatMap[A, B](
      fa: Id[A]
    )(
      f: A => Id[B]
    ): Id[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](
      a: A
    )(
      f: A => Id[Either[A, B]]
    ): Id[B] =
      f(a) match
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => b

  }

  def apply[T, E >: SimpleEnvironment <: Environment](
    f: E ?=> T
  ): T = f(
    using this
  )

  class SimpleBuffer[T] private[SimpleEnvironment] (
    override val buffer: Effect[ArrayBuffer[T]] = new ArrayBuffer[T]()
  ) extends Buffer[T] {
    override type BufferType = ArrayBuffer[T]

    override def add(
      key: String,
      v: T
    ): Effect[Unit] = ???
    override def get(
      key: String
    ): Effect[Option[T]] = ???
  }

  override val bufferFactory: BufferFactory = BufferFactory(
    [x] =>
      (
      ) => new SimpleBuffer[x]()
  )

}
object SimpleEnvironment extends SimpleEnvironment
