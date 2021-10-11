package org.slips

import org.slips.core.HasFact

case class RuleSet[A] private (rules: Map[String, Rule[A]]) {
  def addRule[X, G <: HasFact[_]](rule: Rule.Aux[X, G, A]): RuleSet[A] = copy(rules = rules + (rule.name -> rule))
}
object RuleSet {
  def apply[A](rules: Rule[A]*): RuleSet[A] = RuleSet(rules.map(r => r.name -> r).toMap)
}
