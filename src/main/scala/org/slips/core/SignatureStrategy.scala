package org.slips.core

sealed trait SignatureStrategy {
  inline def createSigned[R](factory: String => R, inline toSign: Any): R
}

object SignatureStrategy {

  /**
    * Creates a signature based on a textual content of the
    * function.
    */
  object Content extends SignatureStrategy {
    inline def createSigned[R](factory: String => R, inline toSign: Any): R = Macros.createSigned(factory, toSign)
  }

  /**
    * Uses hash code of a function to generate a signature
    */
  object HashCode extends SignatureStrategy {
    inline def createSigned[R](factory: String => R, inline toSign: Any): R = factory(toSign.hashCode().toHexString)
  }
}
