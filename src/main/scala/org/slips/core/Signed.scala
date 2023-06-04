package org.slips.core

import org.slips.Signature

object Signed {
  private[slips] inline def apply[R](inline toSign: Any)(res: Signature => R): R = {
    res(Signature.Automatic(content = Macros.sign(toSign), hash = toSign.hashCode().toHexString))
  }
}
