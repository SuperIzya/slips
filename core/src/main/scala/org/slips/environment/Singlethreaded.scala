package org.slips.environment

import org.slips.{Environment, RuleSet}
import org.slips.core.HasFact

class Singlethreaded private(override val rules: RuleSet[Unit]) extends Environment[Unit] {

  override def run: Unit = ()

}

object Singlethreaded {

}
