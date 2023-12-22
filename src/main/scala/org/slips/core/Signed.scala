package org.slips.core

import org.slips.Signature

import scala.deriving.Mirror

trait Signed:
  def signature: Signature
