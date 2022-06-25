package org.slips

import cats.Monad
import cats.data.State
import cats.data.StateT
import org.slips.core.*
import org.slips.core.TypeOps
import org.slips.core.conditions.*
import scala.Tuple.Head
import scala.Tuple.IsMappedBy
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
  given effectMonad: Monad[Effect]

  sealed trait Node[T] {
    val signature: String
    val intake: Node.Intake[_]
    val sink: Node.Sink[T] = Node.Sink()
  }
  object Node          {

    type BuildStep = [x] =>> State[BuildContext, x]

    object BuildStep {
      def apply[T](f: BuildContext ⇒ (BuildContext, T)): BuildStep[T] = State(
        f
      )
      def modify(f: BuildContext ⇒ BuildContext): BuildStep[Unit]     =
        State.modify(f)
      def pure[T](p: T): BuildStep[T]                                 = State.pure(p)
      // def newNode[T](node: Node[T]): BuildStep[Node[T]] = apply(_.add(node))
    }

    sealed trait Builder[N[x] <: Node[x]] {
      extension [T](x: N[T])
        def add: BuildStep[N[T]]
        def equalNode(y: N[T]): Boolean
    }

    object Builder {}

    type Nodes = List[Node[_]]
    object Nodes              {
      val empty: Nodes = List.empty
    }
    sealed trait BuildContext {
      val nodes: Nodes
      def add[T, N[x] <: Node[x]](
        node: N[T]
      )(
        using Builder[N],
        TypeTest[Node[_], N[T]]
      ): BuildStep[N[T]] = {
        nodes
          .collectFirst {
            case n: N[T] if n.signature == node.signature && node.equalNode(n) ⇒
              n
          }
          .map(BuildStep.pure)
          .getOrElse(node.add)

      }
    }
    object BuildContext       {
      def empty: BuildContext = new BuildContext:
        override val nodes: Nodes = Nodes.empty
    }

    case class Sink[T]()

    sealed trait Intake[T]
    case object EmptyIntake                      extends Intake[Any]
    final case class Intake1[T](source: Sink[T]) extends Intake[T]
    final case class Intake2[T1, T2](source1: Sink[T1], source2: Sink[T2])
        extends Intake[T1 *: T2 *: EmptyTuple]

    sealed trait AlphaNode[X, Y]   extends Node[Y]
    sealed trait BetaNode[X, Y, Z] extends Node[Z]
    final case class SourceNode[T](signature: String, f: ContextBuilder.Step[T])
        extends Node[T] {
      override val intake: Intake[Any] = EmptyIntake
    }

    /*
    private def build[T, C[x] <: Condition[x]](condition: C[T]): BuildStep[T] = {
      condition match
        case Condition.FlatMap
    }*/

    /*def dryRun[T](condition: Condition[T]): Result[T] = {
      @tailrec
      def traverse[Q](cond: Condition[Q], col: Nodes): Result[Q] = {
        cond match {
          case Condition.Filter(c, f, ev) =>
            val cRes = traverse(c, col)
            val fRes = traverse(f(ev(cRes.fact)), cRes.nodes)
            fRes.copy(fact = cRes.fact)

        }
      }

      traverse(condition, List.empty)
    }*/
  }

  trait Context[T](facts: Fact.Val[T], values: T) {
    def getValue[Q](fact: Fact[Q]): Effect[(Context[T], Q)]
    def assert[Q](q: Q): Effect[(Context[T], Unit)]
  }

  trait ContextBuilder
  object ContextBuilder {
    type Step[x] = StateT[Effect, ContextBuilder, x]
  }

  type Action[t, q] = StateT[Effect, Context[t], q]
  object Action {
    sealed trait InTuple[T <: Tuple, Q] {
      val pos: Int
    }
    object InTuple                      {
      given [T <: Tuple, Q]: InTuple[Q *: T, Q] with
        override val pos = 0
      given [T <: NonEmptyTuple, Q](
        using
        prev: InTuple[Tuple.Tail[T], Q]
      ): InTuple[T, Q] with
        override val pos: Int = prev.pos + 1
    }

    def apply[T <: Tuple, Q](
      f: Context[T] ⇒ Effect[(Context[T], Q)]
    ): Action[T, Q] = StateT(f)
    def pure[T <: Tuple, Q](q: ⇒ Q): Action[T, Q] = StateT.pure(q)
  }

  final case class Rule[T, Q] private (
    condition: Condition[T],
    action: Fact.Val[T] ⇒ Action[T, Q],
    name: String)
  object Rule   {

    case class ThenBuilder[T <: Tuple] private[Environment] (
      name: String,
      condition: Condition[T]):
      def apply[Q](f: PartialFunction[Fact.Val[T], Action[T, Q]]): Rule[T, Q] =
        Rule(condition, f, name)

    case class WhenBuilder private[Environment] (name: String):
      def apply[T <: Tuple](condition: Condition[T]): ThenBuilder[T] =
        ThenBuilder(name, condition)

    def apply(name: String): WhenBuilder = WhenBuilder(name)

  }

  trait Syntax {
    extension [T <: Tuple](c: Condition[T]) {
      def makeRule(name: String): Rule.ThenBuilder[T] =
        Rule.ThenBuilder(name, c)
    }

  }

  object Syntax extends Syntax {

    trait Conditions {
      def all[T : TypeOps : TypeOps.Size]: Condition.Source[T] =
        Condition.all[T]

      @inline implicit def predicateToCondition(p: Predicate): Condition[Unit] =
        Condition.OpaquePredicate(p)
      /*
      extension [T <: NonEmptyTuple](facts: T) {
        inline def test[Q <: NonEmptyTuple](
          inline t: Q ⇒ Boolean
        )(
          using Q: TypeOps.TupleOps[Q],
          ev1: Q =:= Fact.TInverseMap[T],
          ev2: T =:= Fact.TMap[Q]
        ): Predicate = {
          Fact.fromTuple(ev2(facts)).test(t)
        }
      }*/

      inline implicit def tupleToFact[T <: NonEmptyTuple, Q <: NonEmptyTuple](
        x: T
      )(
        using ev0: Q =:= Fact.TInverseMap[T],
        ev: T =:= Fact.TMap[Q],
        ev1: TypeOps.TupleOps[Q]
      ): Fact[Q] =
        Fact.fromTuple(ev(x))

      inline implicit def liftToLiteralFact[T : Fact.CanBeLiteral : TypeOps](x: T): Fact[T] = Fact.literal(x)

    }
    trait Actions {
      def assert[T <: Tuple, Q](q: Q): Action[T, Unit] = Action(_.assert(q))

      def getValue[T, Q <: Tuple](
        f: Fact[T]
      )(
        using Action.InTuple[Q, T]
      ): Action[Q, T] = Action(_.getValue(f))

      @inline implicit def toAction[T <: Tuple, Q](
        f: Context[T] ?=> Effect[(Context[T], Q)]
      ): Action[T, Q] =
        Action(e ⇒
          f(
            using e
          )
        )
    }

    object Conditions extends Conditions
    object Actions    extends Actions
  }

}
