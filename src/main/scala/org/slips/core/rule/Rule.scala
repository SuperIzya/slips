package org.slips.core.rule

import org.slips.Env
import org.slips.core.build.SelectedPredicatesAndSources
import org.slips.core.conditions.Condition

object Rule {

  trait RuleM {
    private[slips] def sourcesAndPredicates: Env[SelectedPredicatesAndSources]
  }
}
