package org.slips.core.network

case class FactFullSignature(fact: String, predicates: Set[String]) {
  def addPredicate(p: String): FactFullSignature = copy(predicates = predicates + p)
}

object FactFullSignature {
  given factSignOrd: Ordering[FactFullSignature] = (x: FactFullSignature, y: FactFullSignature) => {
    val facts = x.fact.compare(y.fact)
    if (facts != 0) facts
    else {
      val sizes = x.predicates.size.compare(y.predicates.size)
      if (sizes != 0) sizes
      else x.predicates.zip(y.predicates).map { case (a, b) => a.compare(b) }.dropWhile(_ == 0).headOption.getOrElse(0)
    }
  }

  given Ordering[Set[FactFullSignature]] = (x: Set[FactFullSignature], y: Set[FactFullSignature]) => {
    val sizes = x.size.compare(y.size)
    if (sizes != 0) sizes
    else {
      x.zip(y).map { case (a, b) => factSignOrd.compare(a, b) }.dropWhile(_ == 0).headOption.getOrElse(0)
    }
  }

}
