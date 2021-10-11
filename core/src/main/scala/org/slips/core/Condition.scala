package org.slips.core

import scala.quoted.{Expr, Quotes, Type}


sealed trait Condition[-F, A] {
  def flatMap[G, B](other: A => Condition[G, B]): Condition[F with G, B] = Condition.FlatMap(this, other)

  def map[B](f: A => B): Condition[F, B] = Condition.Map(this, f)

  def filter[B](f: (A, B) => Boolean): Option[Condition[F with HasFact[B], A]] = ???

}

object Condition {
  private case class FlatMap[F, A, G, B](self: Condition[F, A], other: A => Condition[G, B])
    extends Condition[F with G, B]

  private case class Map[F, A, B](self: Condition[F, A], f: A => B) extends Condition[F, B]

  case class All[T] private() extends Condition[HasFact[T], T]

  object All {
    def apply[T](using ev: DummyImplicit): All[T] = new All[T]
  }

  case class Not[F, A](condition: Condition[F, A]) extends Condition[F, A]

  private[slips] case class Temp[F, A] private (inner: List[Condition[F, A]], usedSymbols: Set[String]) extends Condition[F, A]
  private[slips] object Temp {
    case class AllConditions[F, A](inner: List[Condition[F, A]]) {
      def usingSymbols(symbols: Set[String]): Temp[F, A] = Temp(inner, symbols)
    }
    def apply[F, A](inner: List[Condition[F, A]]): AllConditions[F, A] = AllConditions(inner)
  }

  private[slips] case class Dummy[F, A](expr: A => Boolean) extends Condition[F, A]
}