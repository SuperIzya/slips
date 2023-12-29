package org.slips

import cats.data.StateT
import org.slips.core.Macros
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.fact.FactOps.TupleOps
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule

package object syntax
    extends FactSyntax
    with ConditionSyntax
    with PredicateSyntax {

  def addFact[Q, T: NotTuple](t: T)(using env: Environment)(using r: env.Rule[Q]): r.Action[Unit] =
    StateT(_.addFact(t))
    
  
}
