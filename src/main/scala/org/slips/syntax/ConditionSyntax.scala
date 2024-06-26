package org.slips.syntax

import org.slips.Environment
import org.slips.EnvRule
import org.slips.core.conditions.*
import org.slips.core.fact.*
import org.slips.core.fact.Fact.Val
import org.slips.core.rule.Rule
import org.slips.core.rule.Rule.RuleAction
import scala.compiletime.summonInline

trait ConditionSyntax {

  extension [T: FactOps](c: Condition[T]) {

    def withFilter(f: Fact.Val[T] => Predicate): Condition[T] =
      Condition.Filter(c, f)

    inline def map[Q, P](f: Fact.Val[T] => Q)(using P: Fact.InverseVal[Q] =:= P): Condition[P] =
      Condition.Map(c, f.andThen(x => P.liftCo(x.asInstanceOf[Fact.Val[Fact.InverseVal[Q]]])))

    def flatMap[Q](f: Fact.Val[T] => Condition[Q]): Condition[Q] =
      Condition.FlatMap(c, f)

    def notExist(using ev: ScalarFact[T]): Condition[T] =
      withFilter(x => Predicate.NotExist(ev(x)))

    def makeRule(using env: Environment)(name: String)(actions: RuleAction[env.Effect, T]): env.Rule[T] =
      Rule(name, c, actions)
  }

  inline def notExists[T : FactOps : ScalarFact]: Condition[T] = all[T].notExist

  inline def all[T : FactOps : ScalarFact]: Condition.Source[T] = Condition.all[T]

}
