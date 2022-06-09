package org.slips
import cats.{Id, Monad}

import scala.annotation.tailrec

object SimpleEnvironment extends Environment {
  override type Effect[x] = cats.Id[x]
  override given effectMonad: Monad[Id] = new Monad[Id] {
    override def pure[T](v: T): Id[T] = v

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = {
      f(a) match
        case Left(a) => tailRecM(a)(f)
        case Right(b) => b
    }
  }

  case class SimpleContext[T <: Tuple](facts: Fact.TMap[T], values: T) extends Context[T](facts, values) {
    override def getValue[Q](fact: Fact[Q])(using ev: Action.InTuple[T, Q]): (Context[T], Q) = {
      (this, values.productElement(facts.toList.indexOf(fact)).asInstanceOf[Q])
    }

    override def assert[Q](q: Q): (Context[T], Unit) = (this, ())
  }

  def apply[T](f: Environment ?=> T): T = f(using SimpleEnvironment)
}
