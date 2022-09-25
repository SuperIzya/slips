package org.slips.core.rule

import org.slips.Env
import org.slips.Environment
import org.slips.core.action.FactId
import org.slips.core.build.Builder
import org.slips.core.build.SelectedPredicatesAndSources
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps

final case class Rule[T: FactOps] private[slips] (
  condition: Condition[T],
  name: String
)(using env: Environment
)(
  action: FactId.Val[T] => env.Action[Unit]
) extends Rule.RuleM {
  override private[slips] val sourcesAndPredicates: Env[SelectedPredicatesAndSources] = Builder
    .selectPredicatesAndSources(condition)
}

object Rule {

  trait RuleM {
    private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources]
  }
}
