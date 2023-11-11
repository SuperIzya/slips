package org.slips

import scala.annotation.showAsInfix
import scala.annotation.targetName

sealed trait Signature { self =>
  @showAsInfix @targetName("append")
  def |+|(postfix: String): Signature = Signature.DerivedUnary(self, _ + postfix)
  def <<(prefix: String): Signature   = Signature.DerivedUnary(self, s => prefix + s)
}

object Signature {
  case class Automatic(hash: String, content: String) extends Signature
  case class Manual(signature: String)                extends Signature

  case class DerivedUnary(orig: Signature, derive: String => String)                              extends Signature
  case class DerivedBinary(left: Signature, right: Signature, derive: (String, String) => String) extends Signature
}
