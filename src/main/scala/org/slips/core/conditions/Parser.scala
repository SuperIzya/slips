package org.slips.core.conditions

import org.slips.Environment
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate

object Parser {

  def apply[T](
    condition: Condition[T]
  ): (
    Context,
    Fact.Val[T]
  ) =
    condition.parse.run(Context.empty).value

  case class Context(
    predicates: Set[Predicate],
    allSources: Set[Condition.Source[?]]
  ) {
    def addPredicate(
      p: Predicate
    ): Context = copy(predicates = predicates + p)
    def addSource[T](
      source: Condition.Source[T]
    ): Context = copy(allSources = allSources + source)
  }

  object Context {
    val empty: Context = Context(Set.empty, Set.empty)
  }
}
