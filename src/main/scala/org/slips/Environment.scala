package org.slips

import cats.Monad
import cats.data.StateT
import cats.syntax.functor.*
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.strategy.PredicateSelection
import scala.language.implicitConversions

trait Environment {

  type Effect[_]

  type Rule = rule.Rule[Effect]

  trait BufferFactory  {
    def create[T]: Buffer[T]
  }
  object BufferFactory {
    def apply(buffer: [x] => () => Buffer[x]): BufferFactory = new BufferFactory {
      def create[T]: Buffer[T] = buffer[T]()
    }
  }

  trait Buffer[T] {
    type BufferType <: Iterable[T]
    protected def buffer: Effect[BufferType]
    def add(key: String, v: T): Effect[Unit]
    def get(key: String): Effect[Option[T]]
    def iterator(using Monad[Effect]): Effect[Iterator[T]] = buffer.map(_.iterator)
  }

  val bufferFactory: BufferFactory
  val predicateSelectionStrategy: PredicateSelection
  val signatureStrategy: Signature.Strategy

  trait ContextBuilder

  object ContextBuilder {
    type Step[x] = StateT[Effect, ContextBuilder, x]
  }

  given effectMonad: Monad[Effect] = compiletime.deferred

}
