package org.slips.core

import org.scalatest.funsuite.AnyFunSuiteLike
import org.slips.SimpleEnvironment
import org.slips.SyntaxTest
import org.slips.core.build.Builder

class BuilderTest extends AnyFunSuiteLike {
  test("SyntaxTest conditions1 parsed and predicates and sources are extracted.") {
    SimpleEnvironment {
      Builder(SyntaxTest.conditions1)
    }
    succeed
  }
}
