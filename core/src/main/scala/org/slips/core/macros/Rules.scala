package org.slips.core.macros

import org.slips.core.Condition

import scala.quoted.{Expr, Quotes, Type}

object Rules {
  def conditions[F, A: Type](value: Expr[Condition[F, A]])(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*
    println(value.show)
    '{ () }
  }

  inline def createFactory[F, A, G, B](inline other: A => Condition[G, B]): Condition[F, A] => Condition[F with G, B] =
    ${ Rules.flatMap[F, A, G, B]('other) }

  def flatMap[F: Type, A: Type, G: Type, B: Type](map: Expr[A => Condition[G, B]])(using quotes: Quotes): Expr[Condition[F, A] => Condition[F with G, B]] = {
    import quotes.reflect.*
    println(s"~~~~~~~~~~~ ${map.show} ~~~~~~~~~~~~~")
    '{ c => Condition.FlatMap[F, A, G, B](c, $map) }
  }
}
