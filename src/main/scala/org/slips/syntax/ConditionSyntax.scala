package org.slips.syntax

import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.Fact.Val
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule
import scala.compiletime.summonInline

trait ConditionSyntax {

  extension [T: FactOps](c: Condition[T]) {

    def withFilter(f: Fact.Val[T] => Predicate): Condition[T] =
      Condition.Filter(c, f)

    def map[Q, P](f: Fact.Val[T] => Q)(using P: Fact.InverseVal[Q] =:= P): Condition[P] =
      Condition.Map(c, f.andThen(x => P.liftCo(x.asInstanceOf[Fact.Val[Fact.InverseVal[Q]]])))

    transparent inline def flatMap[Q](f: Fact.Val[T] => Condition[Q]): Condition[Q] =
      Condition.FlatMap(c, f)

    def notExist(using ev: ScalarFact[T]): Condition[T] =
      withFilter(x => Predicate.NotExist(ev(x)))

    def makeRule(name: String): Rule.Builder[T] = new Rule.Builder(name, c)
  }

  inline def notExists[T : FactOps : ScalarFact]: Condition[T] = all[T].notExist

  inline def all[T : FactOps : ScalarFact]: Condition.Source[T] = Condition.all[T]

}
