package org.slips


trait Environment[A] {
  def run: A
  val rules: RuleSet[A]
}
