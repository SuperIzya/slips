package org.slips

import cats.Id
import cats.Monad
import org.slips.core.build.strategy.PredicateSelection
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait SimpleEnvironment extends Environment {
  type Effect[x] = cats.Id[x]
  val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
  val signatureStrategy: Signature.Strategy          = Signature.Strategy.Content

  override given effectMonad: Monad[Id] = new Monad[Id] {
    def pure[T](v: T): Id[T] = v

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] =
      f(a) match {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => b
      }
  }

  def apply[T, E >: SimpleEnvironment <: Environment](f: E ?=> T): T = f(using this)

  class SimpleBuffer[T] private[SimpleEnvironment] (val buffer: Effect[ArrayBuffer[T]] = new ArrayBuffer[T]())
      extends Buffer[T] {
    type BufferType = ArrayBuffer[T]

    def add(key: String, v: T): Effect[Unit] = ???
    def get(key: String): Effect[Option[T]]  = ???
  }

  val bufferFactory: BufferFactory = BufferFactory([x] => () => new SimpleBuffer[x]())

}
object SimpleEnvironment extends SimpleEnvironment
