package org.slips.core.conditions

import org.slips.{Environment, Signature}
import org.slips.core.SourceLocation
import org.slips.core.fact.{Fact, FactOps, ScalarFact}
import org.slips.core.macros.Macros
import org.slips.core.rule.Rule

trait ConditionSyntax {

  inline def notExists[T: {FactOps, ScalarFact}](using SourceLocation): Condition[T] = all[T].notExist

  inline def all[T : {FactOps, ScalarFact}](using SourceLocation): Condition.All[T] =
    Condition.All[T](Signature.Manual(s"All[${ Macros.signType[T] }]"))

  extension [T: FactOps](c: Condition[T]) {

    def withFilter(f: Fact.Val[T] => Predicate)(using SourceLocation): Condition[T] =
      Condition.Filter(c, f)

    inline def map[Q, P](f: Fact.Val[T] => Q)(using ev: P =:= Fact.InverseVal[Q], ev2: Q =:= Fact.Val[P], P: FactOps[P])(using SourceLocation): Condition[P] =
      Condition.Map(c, f.andThen(ev2))

    def flatMap[Q: FactOps](f: Fact.Val[T] => Condition[Q])(using SourceLocation): Condition[Q] =
      Condition.FlatMap(c, f)

    def notExist(using ev: ScalarFact[T])(using SourceLocation): Condition[T] =
      withFilter(x => Predicate.NotExist(ev(x)))

    def makeRule(name: String): ConditionSyntax.RuleMaker[T] = new ConditionSyntax.RuleMaker(c, name)
  }

}

object ConditionSyntax {
  private[ConditionSyntax] final class RuleMaker[T: FactOps](condition: Condition[T], name: String) {
    inline def apply(using env: Environment)(actions: (rule: Rule[env.Effect]) ?=> rule.Method[T])(
      using SourceLocation
    ): env.Rule =
      Rule(name, condition, actions)
  }
}
