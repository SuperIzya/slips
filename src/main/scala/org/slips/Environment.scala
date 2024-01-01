package org.slips

import cats.Monad
import cats.data.StateT
import cats.syntax.functor.*
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.rule.Rule
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.deriving.Mirror
import scala.language.implicitConversions
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.reflect.TypeTest
import scala.util.NotGiven

trait Environment {

  type Effect[_]

  type Rule[q] = rule.Rule[Effect, q]

  trait BufferFactory  {
    def create[T]: Buffer[T]
  }
  object BufferFactory {
    def apply(
      buffer: [x] => (
      ) => Buffer[x]
    ): BufferFactory = new BufferFactory {
      override def create[T]: Buffer[T] = buffer[T]()
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

  given effectMonad: Monad[Effect]

}
