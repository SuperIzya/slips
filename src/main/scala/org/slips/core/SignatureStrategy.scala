package org.slips.core

import org.slips.Signature
import scala.annotation.tailrec

sealed trait SignatureStrategy {
  protected def select(signature: Signature.Automatic): String

  final def apply(signature: Signature): String = SignatureStrategy.compute(signature, this.select)
}

object SignatureStrategy {

  private case class Call(signature: Signature, nextTransforms: String => List[String => String])

  @tailrec
  private def compute(
    signature: Signature,
    select: Signature.Automatic => String,
    transform: List[String => String] = List.empty,
    stack: List[Call] = List.empty
  ): String = {

    inline def nextStep(res: String): String = {
      if (stack.isEmpty) res
      else {
        val next = stack.head
        compute(next.signature, select, next.nextTransforms(res), stack.tail)
      }
    }

    signature match
      case s: Signature.Automatic                       =>
        val res = transform.foldLeft(select(s))((s, t) => t(s))
        nextStep(res)
      case Signature.Manual(s)                          =>
        val res = transform.foldLeft(s)((s, t) => t(s))
        nextStep(res)
      case Signature.DerivedUnary(orig, derive)         => compute(orig, select, derive +: transform, stack)
      case Signature.DerivedBinary(left, right, derive) =>
        compute(left, select, List.empty, Call(right, s => ((x: String) => derive(s, x)) +: transform) +: stack)
  }

  /**
    * Creates a signature based on a textual content of the
    * function.
    */
  object Content extends SignatureStrategy {
    override protected def select(signature: Signature.Automatic): String = signature.content
  }

  /**
    * Uses hash code of a function to generate a signature
    */
  object HashCode extends SignatureStrategy {
    override protected def select(signature: Signature.Automatic): String = signature.hash
  }

}
