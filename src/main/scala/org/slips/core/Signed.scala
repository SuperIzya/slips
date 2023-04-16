package org.slips.core

object Signed {
  private[slips] inline def createObject[R](res: String => R, inline toSign: Any): SignatureStrategy ?=> R =
    strategy ?=> strategy.createSigned(res, toSign)
}
