package org.slips

import org.slips.core.Signed
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.data.*

package object generator {
  type Alpha[D <: Data] = Fact[D] => Signed[Predicate]

  object Alpha {
    def apply[D <: Data](f: Fact[D] => Signed[Predicate]): Alpha[D] = f
  }

  type AlphaGen[D <: Data] = DGen[Alpha[D]]
}
