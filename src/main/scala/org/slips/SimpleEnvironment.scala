package org.slips
import cats.{Id, Monad}
import org.slips.core.Fact

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

  case class SimpleContext[T](facts: Fact.Val[T], values: T) extends Context[T](facts, values) {
    override def getValue[Q](fact: Fact[Q]): (Context[T], Q) = ???

    override def assert[Q](q: Q): (Context[T], Unit) = (this, ())
  }

  def apply[T](f: Environment ?=> T): T = f(using SimpleEnvironment)
}
