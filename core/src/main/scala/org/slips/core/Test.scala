package org.slips.core


object Test {
  import org.slips.core.macros.Predicates._
  inline def apply[A](inline cond: Boolean): Condition[HasFact[A], A] = ${ singleFact('cond)}
}