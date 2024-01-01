package org.slips.syntax

import cats.data.State
import org.slips.Signature
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import scala.annotation.targetName
import scala.language.implicitConversions

trait PredicateSyntax {

  implicit inline def convertToCondition(p: Predicate): Condition[Unit] =
    Condition.Opaque(p)

  extension (p: Predicate) {
    def and(s: Predicate): Predicate = Predicate.And(p, s)
    def or(s: Predicate): Predicate  = Predicate.Or(p, s)
    def not: Predicate               = Predicate.Not(p)

    @targetName("and")
    inline def &&(s: Predicate): Predicate = and(s)
    @targetName("or")
    inline def ||(s: Predicate): Predicate = or(s)

    @targetName("not")
    inline def unary_! : Predicate = not

    private[slips] def toKNF: Predicate = PredicateSyntax.toKNF(p).runA(PredicateSyntax.Stack.empty).value

    private[slips] def alphaSources: Set[Fact.Source] = p.facts.flatMap(_.alphaSources)
  }

  extension [T](t: Predicate.Test[T]) {

    def signed(signature: => String)(using T: FactOps[T]): Test[T] =
      signed(Signature.Manual(signature))

    def signed(signature: Signature)(using T: FactOps[T]): Test[T] =
      t.copy(signature = signature)

    def negate(using T: FactOps[T]): Test[T] = t.copy(
      signature = t.signature.prepend("!"),
      test = x => !t.test(x)
    )
  }

}

object PredicateSyntax {
  private type Test     = Predicate.Test[?]
  private type Exist    = Predicate.Exist[?]
  private type NotExist = Predicate.NotExist[?]

  type Stack   = List[Operation]
  type Step[T] = State[Stack, T]

  def toKNF(p: Predicate): Step[Predicate] = p match {
    case Predicate.And(left, right)   =>
      for {
        l <- toKNF(left)
        r <- toKNF(right)
      } yield l && r
    case Predicate.Or(left, right)    =>
      for {
        l   <- toKNF(left)
        r   <- toKNF(right)
        res <- (l, r) match {
          case (Predicate.And(al, ar), r) =>
            for {
              a <- toKNF(al || r)
              b <- toKNF(ar || r)
            } yield a && b
          case (l, Predicate.And(al, ar)) =>
            for {
              a <- toKNF(al || l)
              b <- toKNF(ar || l)
            } yield a && b
          case _                          => State.pure(l || r)
        }
      } yield res
    case Predicate.Not(p)             =>
      p match {
        case Predicate.And(left, right)   => toKNF(!left || !right)
        case Predicate.Or(left, right)    => toKNF(!left && !right)
        case Predicate.Not(p)             => toKNF(p)
        case t: (Test | Exist | NotExist) => State.pure(t.not)
      }
    case t: (Test | Exist | NotExist) => State.pure(t)
  }

  sealed trait Operation

  object Operation {
    case class Result[T](result: T)                          extends Operation
    case class WaitForRight[T](left: T, reduce: (T, T) => T) extends Operation
    case class WaitForLeft[T](reduce: (T, T) => T)           extends Operation
    case class FlatMap[T](f: T => Step[T])                   extends Operation
  }

  object Stack {
    def empty: Stack                   = List.empty
    def push(o: Operation): Step[Unit] = State.modify(o +: _)
    def pop: Step[Option[Operation]]   = State { s => s.headOption.fold((s, None))(head => s.tail -> Some(head)) }
  }
}
