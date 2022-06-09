package org.slips


import cats.data.{State, StateT}
import cats.{Monad, Monoid}

import scala.Tuple.IsMappedBy
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.quoted.{Expr, Quotes, Type}
import scala.util.NotGiven

trait Environment {
  type Effect[_]
  given effectMonad: Monad[Effect]

  sealed trait Rep[I] {
    private def effectTest[T <: Tuple](f: T => Boolean): T => Effect[Boolean] = {
      a => effectMonad.pure(f(a))
    }
    def =!= (other: Rep[I]): Predicate = Predicate.Test[(I, I)]((this, other), effectTest(_ != _))
    def === (other: Rep[I]): Predicate = Predicate.Test[(I, I)]((this, other), effectTest(_ == _))
    def map(f: I => Effect[Boolean]): Predicate = Predicate.Test[Tuple1[I]](Tuple1(this), x => f(x._1))
  }
  object Rep {
    final case class Extractor[T, I](source: Fact[T], extract: T => Effect[I]) extends Rep[I]

    final case class Literal[I](value: Effect[I]) extends Rep[I]
    @inline implicit def liftToLiteral[I](v: => I): Literal[I] =
      Literal(effectMonad.pure(v))

  }
  sealed trait Fact[T] extends Rep[T] {
    def get[I](f: T => Effect[I]): Rep[I] = Rep.Extractor[T, I](this, f)
  }
  object Fact {
    final case class Initial[T](source: Source[T])(using NotGiven[T <:< Tuple]) extends Fact[T]
    final case class Path[T]()

    type TMap[x <: Tuple] = Tuple.Map[x, Fact]
    type TInverseMap[x <: Tuple] = Tuple.InverseMap[x, Fact]
    type TIsMapped[x <: Tuple] = Tuple.IsMappedBy[Fact][x]
  }

  sealed trait Node[T]
  object Node {
    sealed trait Intake[T] {
      val source: Node[_]

    }

    type Nodes = List[Node[_]]
    case class BuildContext()
    case class Result[T](nodes: Nodes, fact: Fact[T])

    type BuildStep[x] = State[BuildContext, Result[x]]
    trait Builder[C[_] <: Condition[_]] {
      def build[T](c: C[T]): BuildStep[T]
    }
    /*
    given flatMap: Builder[Condition.FlatMap[_, ?]]

    private def build[T, C[x] <: Condition[x]](condition: C[T])(using B: Builder[C]): BuildStep[T] = {
      B.build(condition)
    }
*/
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

  sealed trait Condition[T] {
    def flatMap[Q](f: T => Condition[Q]): Environment ?=> Condition[Q] = Condition.FlatMap[T, Q](this, f)
    def map[Q](f: T => Q): Environment ?=> Condition[Q] = Condition.Map(this, f)
    def withFilter[Q](f: T => Predicate)(using ev: T =:= Fact[Q]): Environment ?=> Condition[Fact[Q]] =
      Condition.Filter[Q](ev.substituteCo(this), x => f(ev.flip(x)))

  }
  object Condition {
    private[slips] final case class Map[T, Q] (cond: Condition[T], f: T => Q)
      extends Condition[Q]
    private[slips] final case class FlatMap[T, Q](left: Condition[T], f: T => Condition[Q])
      extends Condition[Q]
    private[slips] final case class Filter[T](cond: Condition[Fact[T]], f: Fact[T] => Predicate)
      extends Condition[Fact[T]]
    private[slips] final case class TupleMap[T <: Tuple, Q <: Tuple](left: Condition[T], f: Fact.TMap[T] => Q)
      extends Condition[Fact.TInverseMap[Q]]

  }

  sealed trait Source[T] extends Condition[Fact[T]] {
    private [slips] def dryRun: Fact[T] = Fact.Initial(this)
  }

  object Source {
    final case class All[T] private[Environment] () extends Source[T]
  }

  sealed trait Predicate extends Condition[Fact[Boolean]] {
    def and (other: Predicate): Predicate = Predicate.And(this, other)
    def or (other: Predicate): Predicate = Predicate.Or(this, other)
    def not: Predicate = Predicate.Not(this)

    def && (other: Predicate): Predicate = and(other)
    def || (other: Predicate): Predicate = or(other)
  }

  object Predicate {
    final case class Test[T <: Tuple](args: Tuple.Map[T, Rep], operand: T => Effect[Boolean]) extends Predicate

    final case class And(left: Predicate, right: Predicate) extends Predicate
    final case class Or(left: Predicate, right: Predicate) extends Predicate
    final case class Not private(p: Predicate) extends Predicate
    object Not {
      def apply(p: Predicate)(using DummyImplicit): Predicate = p match
        case Not(pp) => pp
        case _ => new Not(p)
    }
  }

  trait Context[T <: Tuple](facts: Fact.TMap[T], values: T) {
    def getValue[Q](fact: Fact[Q])(using Action.InTuple[T, Q]): Effect[(Context[T], Q)]
    def assert[Q](q: Q): Effect[(Context[T], Unit)]
  }

  type Action[t <: Tuple, q] = StateT[Effect, Context[t], q]
  object Action {
    sealed trait InTuple[T <: Tuple, Q](val pos: Int)
    object InTuple {
      implicit def trivial[T <: Tuple, Q]: InTuple[Q *: T, Q] = new InTuple[Q *: T, Q](0) {}
      implicit def gen[T <: NonEmptyTuple, Q](using prev: InTuple[Tuple.Tail[T], Q]): InTuple[T, Q] =
        new InTuple[T, Q](prev.pos + 1) {}
    }

    def apply[T <: Tuple, Q](f: Context[T] => Effect[(Context[T], Q)]): Action[T, Q] = StateT(f)
    def pure[T <: Tuple, Q](q: => Q): Action[T, Q] = StateT.pure(q)
  }

  final case class Rule[T <: Tuple, Q] private(
    condition: Condition[T],
    action: T => Action[Fact.TInverseMap[T], Q],
    name: String
  )
  object Rule {

    case class ThenBuilder[T <: Tuple] private[Environment] (name: String, condition: Condition[T]):
      def apply[Q](f: PartialFunction[T, Action[Fact.TInverseMap[T], Q]]): Rule[T, Q] = Rule[T, Q](condition, f, name)

    case class WhenBuilder private[Environment] (name: String):
      def apply[T <: Tuple](condition: Condition[T]): ThenBuilder[T] = ThenBuilder(name, condition)

    def apply(name: String): WhenBuilder = WhenBuilder(name)

  }


  object Syntax {

    def all[T]: Source[T] = Source.All()

    def assert[T <: Tuple, Q](q: Q): Action[T, Unit] = Action(_.assert(q))
    def getValue[T, Q <: Tuple](f: Fact[T])(using Action.InTuple[Q, T]): Action[Q, T] = Action(_.getValue(f))

    @inline implicit def toAction[T <: Tuple, Q](f: Context[T] ?=> Effect[(Context[T], Q)]): Action[T, Q] =
      Action(e => f(using e))


    extension [T <: Tuple](c: Condition[T]) {
      def makeRule(name: String): Rule.ThenBuilder[T] = Rule.ThenBuilder(name, c)
    }
  }


}
