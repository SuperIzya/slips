package org.slips

import org.slips.core.InFacts



trait Context[A] {

  def addFact[C](fact: C): A
  def removeFact[C](fact: C): A
  def updateFact[C](oldFact: C, newFact: C): A
}
