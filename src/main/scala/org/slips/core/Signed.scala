package org.slips.core

import scala.deriving.Mirror

trait Signed:
  def signature: String

