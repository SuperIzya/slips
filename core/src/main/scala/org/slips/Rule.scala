package org.slips

import org.slips.core.macros.Rules
import org.slips.core.{Condition, HasFact}

sealed trait Rule[A] {
  type Argument
  type Facts <: HasFact[_]

  val action: Argument => Context[A] => A
  val condition: Condition[Facts, Argument]
  val name: String

}

object Rule {
  type Aux[C, F <: HasFact[_], A] = Rule[A] {
    type Argument = C
    type Facts = F
  }

  inline private def collectConditions[F, C](inline c: Condition[F, C]): Unit = ${ Rules.conditions('c) }

  inline def apply[C, F <: HasFact[_], A](n: String)
                                         (inline c: Condition[F, C])
                                         (a: C => Context[A] => A): Rule.Aux[C, F, A] = {
    collectConditions(c)
    new Rule[A] {
      override type Argument = C
      override type Facts = F
      val action: C => Context[A] => A = a
      val condition: Condition[F, C] = c
      val name: String = n
    }
  }


}