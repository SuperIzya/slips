package org.slips.core

import org.slips.Signature

object Signed {
  private[slips] inline def apply[R](inline toSign: Any)(res: Signature => R): Signed[R] = s ?=>
    s match {
      case SignatureStrategy.Content  => SignatureStrategy.Content.sign(res, toSign)
      case SignatureStrategy.HashCode => SignatureStrategy.HashCode.sign(res, toSign)
    }
}
