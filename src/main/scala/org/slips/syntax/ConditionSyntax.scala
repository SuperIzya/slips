package org.slips.syntax

import org.slips.Environment
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.fact.Fact.Val
import org.slips.core.rule.Rule
import org.slips.syntax.ConditionSyntax.RuleMaker

trait ConditionSyntax {

  extension [T: FactOps](c: Condition[T]) {

    def withFilter(f: Fact.Val[T] => Predicate): Condition[T] =
      Condition.Filter(c, f)

    inline def map[Q, P](f: Fact.Val[T] => Q)(using ev: Q =:= Fact.Val[P], P: FactOps[P]): Condition[P] =
      Condition.Map(c, f.andThen(ev))

    def flatMap[Q: FactOps](f: Fact.Val[T] => Condition[Q]): Condition[Q] =
      Condition.FlatMap(c, f)

    def notExist(using ev: ScalarFact[T]): Condition[T] =
      withFilter(x => Predicate.NotExist(ev(x)))

    def makeRule(name: String): RuleMaker[T] = new RuleMaker(c, name)
  }

  inline def notExists[T : FactOps : ScalarFact]: Condition[T] = all[T].notExist

  inline def all[T : FactOps : ScalarFact]: Condition.Source[T] = Condition.all[T]

}

object ConditionSyntax {
  final class RuleMaker[T: FactOps](condition: Condition[T], name: String) {
    def apply(using env: Environment)(actions: (rule: Rule[env.Effect]) ?=> rule.Method[T]): env.Rule =
      Rule(name, condition, actions)
  }
}
