package org.slips

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.kernel.Order
import org.slips.core.Signed

sealed trait Signature { self =>
  def append(postfix: String): Signature = Signature.DerivedUnary(self, _ + postfix)
  def prepend(prefix: String): Signature = Signature.DerivedUnary(self, s => prefix + s)
}

object Signature {
  opaque type SignatureTuple = NonEmptyList[Signature]
  object SignatureTuple {
    def first(s: Signature): SignatureTuple = NonEmptyList.one(s)

  }

  extension (t: SignatureTuple) {
    def toSignature: Signature                = TupleSign(t)
    def append(s: Signature): SignatureTuple  = t.append(s)
    def prepend(s: Signature): SignatureTuple = t.prepend(s)
  }

  given order: Order[Signature] with {
    override def compare(x: Signature, y: Signature): Int =
      x.hashCode().compare(y.hashCode())
  }

  opaque type SignatureSet = NonEmptySet[Signature]
  object SignatureSet {
    def first(s: Signature): SignatureSet = NonEmptySet.one(s)

  }

  extension (s: SignatureSet) {
    def toSignature: Signature = SetSign(s)
  }

  // case class Automatic(hash: String, content: String)                        extends Signature
  case class Manual(signature: String)                                       extends Signature
  private case class DerivedUnary(orig: Signature, derive: String => String) extends Signature
  private case class DerivedBinary(left: Signature, right: Signature, derive: (String, String) => String)
      extends Signature

  private case class TupleSign(sign: SignatureTuple) extends Signature
  private case class SetSign(sig: SignatureSet)      extends Signature

  def derivedUnary(signature: Signature, derive: String => String): Signature =
    DerivedUnary(signature, derive)
  def derivedUnary(orig: Signed, derive: String => String): Signature         =
    derivedUnary(orig.signature, derive)

  def derivedBinary(left: Signed, right: Signed, derive: (String, String) => String): Signature =
    DerivedBinary(left.signature, right.signature, derive)

  def collect(signatures: List[Signature]): Signature = {
    val elements = signatures.reduceOption { DerivedBinary(_, _, (a, b) => s"$a, $b") }
    elements.fold(Manual("()")) { DerivedUnary(_, s => s"($s)") }
  }

  def compute(sig: Signature): String = sig match {
    case Manual(signature)                  => signature
    case DerivedUnary(orig, derive)         => derive(compute(orig))
    case DerivedBinary(left, right, derive) => derive(compute(left), compute(right))
    case TupleSign(sign)                    => sign.toList.map(compute).mkString("(", ", ", ")")
    case SetSign(sig)                       => sig.toSortedSet.map(compute).mkString("{", ", ", "}")
  }
}
