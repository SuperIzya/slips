package org.slips.core

import org.slips.Signature
import scala.deriving.Mirror

trait WithSignature {
  type Self <: this.type
  def signature: Signature
}
