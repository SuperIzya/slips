package org.slips.core

import org.slips.Signature

sealed trait SignatureStrategy {

  inline def sign[R](factory: Signature => R, inline toSign: Any): R

  inline def apply[T](f: Signed[T]): T = f(using this)
}

object SignatureStrategy {

  /**
    * Creates a signature based on a textual content of the
    * function.
    */
  object Content extends SignatureStrategy {
    override inline def sign[R](factory: Signature => R, inline toSign: Any): R = Macros.createSigned(factory, toSign)
  }

  /**
    * Uses hash code of a function to generate a signature
    */
  object HashCode extends SignatureStrategy {
    override inline def sign[R](factory: Signature => R, inline toSign: Any): R = factory(toSign.hashCode().toHexString)
  }

}
