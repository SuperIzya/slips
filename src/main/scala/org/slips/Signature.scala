package org.slips

sealed trait Signature

object Signature {
  case class Automatic(hash: String, content: String) extends Signature
  case class Manual(signature: String)                extends Signature

  case class DerivedUnary(orig: Signature, derive: String => String)                              extends Signature
  case class DerivedBinary(left: Signature, right: Signature, derive: (String, String) => String) extends Signature
}
