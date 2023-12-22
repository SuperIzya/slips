package org.slips.syntax

import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule
import scala.compiletime.erasedValue
import scala.compiletime.summonInline

trait ConditionSyntax {
  extension [T: FactOps](c: Condition[T]) {
    inline def makeRule(name: String): Rule.Builder[T] =
      new Rule.Builder(name, c)

    transparent inline def map[Q](f: Fact.Val[T] => Q) =
      inline erasedValue[Q] match {
        case _: Fact.Val[X] =>
          Condition.Map(c, f.andThen(_.asInstanceOf[Fact.Val[X]]))
        case _: Predicate   =>
          withFilter(f.andThen(_.asInstanceOf[Predicate]))
      }

    inline def flatMap[Q](f: Fact.Val[T] => Condition[Q]): Condition[Q] =
      Condition.FlatMap(c, f)

    inline def notExist(using ev: ScalarFact[T]): Condition[T] =
      withFilter(x => Predicate.NotExist(ev(x)))

    inline def withFilter(f: Fact.Val[T] => Predicate): Condition[T] =
      Condition.Filter(c, f)
  }

  inline def notExists[T : FactOps : ScalarFact]: Condition[T] = all[T].notExist

  inline def all[T : FactOps : ScalarFact]: Condition.Source[T] = Condition.all[T]

}
