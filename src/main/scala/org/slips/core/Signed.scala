package org.slips.core

import scala.deriving.Mirror

trait Signed {
  def signature: String
  def signed(signature: String): this.type = this
}
