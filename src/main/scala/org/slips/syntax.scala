package org.slips

import cats.data.StateT
import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.conditions.ConditionSyntax
import org.slips.core.conditions.PredicateSyntax
import org.slips.core.fact.Syntax as FactSyntax

object syntax extends ConditionSyntax with FactSyntax with PredicateSyntax {

  def addFact[T: NotTuple](t: T)(using env: Environment)(using r: env.Rule): r.Action[Unit] =
    StateT(_.addFact(t))

}
