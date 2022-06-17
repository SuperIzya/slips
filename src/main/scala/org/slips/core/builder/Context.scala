package org.slips.core.builder

import org.slips.core.conditions.Predicate

case class Context(predicates: List[Predicate]) {
  def addPredicate(p: Predicate): Context = copy(predicates = p +: predicates)
}

object Context {
  val empty = Context(List.empty)
}
