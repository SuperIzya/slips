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
      val (predicatesClean, sourcesClean) = SEClean {
        Builder.sourcesAndPredicates(SyntaxTest.conditions1)
      }

      sourcesClean should have size 2
      predicatesClean should have size 6
    }

    "PredicateSelection.Keep" in {
      object SEKeep extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Keep
      }

      val (predicatesKeep, sourcesKeep) = SEKeep {
        Builder.sourcesAndPredicates(SyntaxTest.conditions1)
      }

      sourcesKeep should have size 2
      predicatesKeep should have size 9
    }
  }
}
