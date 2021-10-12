package org.slips.core

import org.slips.core.macros.Rules

import scala.quoted.{Expr, Quotes, Type}


sealed trait Condition[-F, A] {
  inline def flatMap[G, B](inline other: A => Condition[G, B]): Condition[F with G, B] = {
    val create: Condition[F, A] => Condition[F with G, B] = Rules.createFactory(other)
    create(this)
  }


  def map[B](f: A => B): Condition[F, B] = Condition.MapCondition(this, f)

}

private[slips] object Condition {


  private[slips] type UsedSymbols = Set[String]
  private[slips] object UsedSymbols {
    val empty: UsedSymbols = Set.empty
  }
  private[slips] case class FlatMap[F, A, G, B] private (other: A => Condition[G, B], self: Condition[F, A])
    extends Condition[F with G, B]
  object FlatMap {

    def apply[F, A, G, B](self: Condition[F, A], other: A => Condition[G, B]): FlatMap[F, A, G, B] = {

      FlatMap(other, self)
    }
  }

  private[slips] case class MapCondition[F, A, B](self: Condition[F, A], f: A => B) extends Condition[F, B]

  case class All[T] private() extends Condition[HasFact[T], T]

  object All {
    def apply[T](using ev: DummyImplicit): All[T] = new All[T]
  }

  case class Not[F, A](condition: Condition[F, A]) extends Condition[F, A]

  private[slips] case class Temp[F, A] private (usedSymbols: UsedSymbols, inner: List[Condition[F, A]]) extends Condition[F, A]
  private[slips] object Temp {
    def apply[F, A](inner: List[Condition[F, A]], usedSymbols: UsedSymbols)(using DummyImplicit): Temp[F, A] =
      Temp(usedSymbols, inner)
  }

  private[slips] case class Dummy[F, A](expr: A => Boolean) extends Condition[F, A]
}