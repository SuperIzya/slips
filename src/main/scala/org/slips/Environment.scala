package org.slips

import cats.Monad
import cats.data.StateT
import cats.syntax.functor.*
import org.slips.core.*
import org.slips.core.FactSize
import org.slips.core.build.BuildContext
import org.slips.core.build.BuildStep
import org.slips.core.build.Node
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import scala.Tuple._
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
  type Action[t, q] = StateT[Effect, Context[t], q]

  trait BufferFactory  {
    def create[T]: Buffer[T]
  }
  object BufferFactory {
    def apply(buffer: [x] => () => Buffer[x]): BufferFactory = new BufferFactory {
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

  trait Context[T](facts: Fact.Val[T], values: T) {
    def getValue[Q](fact: Fact[Q]): Effect[(Context[T], Q)]
    def assert[Q](q: Q): Effect[(Context[T], Unit)]
    def remove[Q](facts: Fact.Val[Q]): Effect[(Context[T], Unit)]
  }

  trait ContextBuilder

  trait Syntax {
    extension [T <: Tuple](c: Condition[T]) {
      def makeRule(name: String): Rule.ThenBuilder[T] =
        Rule.ThenBuilder(name, c)
    }

  }

  final case class Rule[T, Q] private (
    condition: Condition[T],
    action: Fact.Val[T] => Action[T, Q],
    name: String
  )

  object ContextBuilder {
    type Step[x] = StateT[Effect, ContextBuilder, x]
  }

  object Action {
    def apply[T <: Tuple, Q](
      f: Context[T] => Effect[(Context[T], Q)]
    ): Action[T, Q] = StateT(f)

    def pure[T <: Tuple, Q](q: => Q): Action[T, Q] = StateT.pure(q)

    sealed trait InTuple[T <: Tuple, Q] {
      val pos: Int
    }

    object InTuple {
      given [T <: Tuple, Q]: InTuple[Q *: T, Q] with
        override val pos = 0
      given [T <: NonEmptyTuple, Q](using
        prev: InTuple[Tuple.Tail[T], Q]
      ): InTuple[T, Q] with
        override val pos: Int = prev.pos + 1
    }
  }

  object Rule {

    def apply(name: String): WhenBuilder = WhenBuilder(name)

    case class ThenBuilder[T <: Tuple] private[Environment] (
      name: String,
      condition: Condition[T]
    ):
      def apply[Q](f: PartialFunction[Fact.Val[T], Action[T, Q]]): Rule[T, Q] =
        Rule(condition, f, name)

    case class WhenBuilder private[Environment] (name: String):
      def apply[T <: Tuple](condition: Condition[T]): ThenBuilder[T] =
        ThenBuilder(name, condition)

  }

  object Syntax extends Syntax {

    trait Conditions {
      inline def all[T : TypeOps : FactSize]: Condition.Source[T] = Condition.all[T]

      inline implicit def predicateToCondition(p: Predicate): Condition[Unit] =
        Condition.OpaquePredicate(p)

      inline implicit def tupleToFact[T <: NonEmptyTuple, Q <: NonEmptyTuple](
        x: T
      )(using ev0: Q =:= Fact.TInverseMap[T],
        ev: T =:= Fact.TMap[Q],
        ev1: TypeOps.TupleOps[Q]
      ): Fact[Q] =
        Fact.fromTuple(ev(x))

      inline implicit def liftToLiteralFact[T : Fact.CanBeLiteral : TypeOps](x: T): Fact[T] = Fact.literal(x)

      extension [T](fact: Fact[T]) {
        inline def value[I: TypeOps](inline f: T => I): Fact[I] =
          Macros.createSigned[Fact.Map[T, I]](
            s => Fact.Map(s"${ fact.signature } => $s", f, fact),
            f
          )
      }

    }
    trait Actions {
      def assert[T <: Tuple, Q](q: Q): Action[T, Unit] = Action(_.assert(q))

      extension [T](f: Fact[T]) {
        def value[Q <: Tuple](using Action.InTuple[Q, T]): Action[Q, T]     = Action(_.getValue(f))
        def remove[Q <: Tuple](using Action.InTuple[Q, T]): Action[Q, Unit] = Action(_.remove(f.toVal))
      }

      @inline implicit def toAction[T <: Tuple, Q](
        f: Context[T] ?=> Effect[(Context[T], Q)]
      ): Action[T, Q] =
        Action(e =>
          f(
            using e
          )
        )
    }

    object Conditions extends Conditions
    object Actions    extends Actions
  }

  given effectMonad: Monad[Effect]

}
