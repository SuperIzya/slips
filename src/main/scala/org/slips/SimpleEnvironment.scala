package org.slips

import cats.Id
import cats.Monad
import org.slips.core.Fact
import org.slips.core.build.strategy.PredicateSelection
import scala.annotation.tailrec

trait SimpleEnvironment  extends Environment {
  override type Effect[x] = cats.Id[x]
  override given effectMonad: Monad[Id] = new Monad[Id] {
    override def pure[T](v: T): Id[T] = v

    override def flatMap[A, B](fa: Id[A])(f: A ⇒ Id[B]): Id[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A ⇒ Id[Either[A, B]]): Id[B] = {
      f(a) match
        case Left(a)  ⇒ tailRecM(a)(f)
        case Right(b) ⇒ b
    }
  }

  override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean

  case class SimpleContext[T](facts: Fact.Val[T], values: T)
      extends Context[T](facts, values) {
    override def getValue[Q](fact: Fact[Q]): (Context[T], Q) = ???

    override def assert[Q](q: Q): (Context[T], Unit) = (this, ())
  }

  def apply[T](f: Environment ?=> T): T = f(
    using this
  )
}
object SimpleEnvironment extends SimpleEnvironment
