package org.slips

import cats.Monad
import cats.data.StateT
import cats.syntax.functor.*
import org.slips.core.*
import org.slips.core.action.FactId
import org.slips.core.action.FactIdOps
import org.slips.core.build.*
import org.slips.core.build.strategy.AlphaNodeStrategy
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.predicates.Predicate
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

  type Action[q] = action.Action[Effect, q]
  object Action        {
    type Context   = action.Context[Effect]
    type Method[t] = Context => Effect[(Context, t)]
  }
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
  val alphaNodeStrategy: AlphaNodeStrategy

  trait ContextBuilder

  object ContextBuilder {
    type Step[x] = StateT[Effect, ContextBuilder, x]
  }

  given effectMonad: Monad[Effect]

}

object Environment {

  trait TSyntax {

    trait TConditions {
      inline def all[T: FactOps](using NotGiven[T <:< Tuple]): Condition.Source[T] = Condition.all[T]

      inline implicit def predicateToCondition(p: Predicate): Condition[Unit] =
        Condition.OpaquePredicate(p)

      inline implicit def tupleToFact[T <: NonEmptyTuple, Q <: NonEmptyTuple](
        x: T
      )(using ev0: Q =:= Fact.TInverseMap[T],
        ev: T =:= Fact.TMap[Q],
        ev1: FactOps.TupleOps[Q]
      ): Fact[Q] =
        Fact.fromTuple(ev(x))

      inline implicit def liftToLiteralFact[T : Fact.CanBeLiteral : FactOps](x: T): Fact[T] = Fact.literal(x)

      extension [T](fact: Fact[T]) {
        inline def value[I: FactOps](inline f: T => I): Fact[I] =
          Macros.createSigned[Fact.Map[T, I]](
            s => Fact.Map(s"${ fact.signature } => $s", f, fact),
            f
          )
      }

      def notExists[T](f: Fact.Val[T] => Predicate)(using F: FactOps[T]): Condition[Unit] =
        F.allCondition.flatMap(v => Condition.OpaquePredicate(f(v)))

    }

    trait TActions {
      def assert[Q: NotTuple](q: Q)(using env: Environment): env.Action[Unit] =
        action.Action(_.assert(q))

      extension [Q: NotTuple](f: FactId[Q]) {

        // TODO: Make proper monadic error here and in all action section of the rule.
        def valueGet[T](using env: Environment, ev: Q =:= Option[T]): env.Action[T] =
          action.Action(_.getValue(f)) map { (q: Q) => ev(q).get }

        def remove(using env: Environment): env.Action[Unit] = action.Action(_.remove(f))

      }

      extension [T](f: FactId.Val[T])(using T: FactIdOps[T]) {
        // TODO: Fix it!
        def mapN(using env: Environment)(map: T => env.Action[Unit]): env.Action[Unit] =
          T.valueAction(f) flatMap map

      }

      extension [T](c: Condition[T])(using F: FactOps[T], env: Environment)
        def makeRule(name: String)(f: FactId.Val[T] => env.Action[Unit]): Rule[T] =
          Rule(c, name)(f)

    }
  }

  object Syntax extends TSyntax {

    object Conditions extends TConditions

    object Actions extends TActions
  }

}
