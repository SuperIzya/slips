package org.slips.core

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.*
import org.slips.SimpleEnvironment
import org.slips.SyntaxTest
import org.slips.core.build.Builder
import org.slips.core.build.strategy.PredicateSelection

class BuilderTest extends AnyFreeSpec with Matchers {
  "Condition parser should find all predicates and sources with respect to Environment.predicateSelectionStrategy" - {
    "PredicateSelection.Clean" in {
      object SEClean extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val (predicates, sources) = SEClean {
        Builder.sourcesAndPredicates(SyntaxTest.conditions1)
      }

      sources should have size 2
      predicates should have size 10
    }

    "PredicateSelection.Keep" in {
      object SEKeep extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Keep
      }

      val (predicates, sources) = SEKeep {
        Builder.sourcesAndPredicates(SyntaxTest.conditions1)
      }

      sources should have size 3
      predicates should have size 12
    }
  }
}
