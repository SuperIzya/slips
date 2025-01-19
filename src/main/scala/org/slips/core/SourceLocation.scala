package org.slips.core

import org.slips.core.macros.Macros

final case class SourceLocation(path: String, line: Int)

object SourceLocation {
  inline given SourceLocation = ${ Macros.sourceLocation }
}
