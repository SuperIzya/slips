package org.slips

import cats.data.NonEmptyList
import org.slips.core.WithSignature
import org.slips.core.macros.Macros
import scala.annotation.targetName

sealed trait Signature { self =>
  def append(postfix: String): Signature = Signature.DerivedUnary(self, _ + postfix)
  def prepend(prefix: String): Signature = Signature.DerivedUnary(self, s => prefix + s)
}

object Signature {
  case class Automatic private[Signature] (hash: String, content: String) extends Signature
  case class Manual(signature: String)                extends Signature

  case class DerivedUnary(orig: Signature, derive: String => String)                              extends Signature
  case class DerivedBinary(left: Signature, right: Signature, derive: (String, String) => String) extends Signature

  case class SignatureTuple(signature: NonEmptyList[Signature]) extends Signature
  object SignatureTuple {
    def apply(s: Signature): SignatureTuple = SignatureTuple(NonEmptyList.one(s))

  }
  
  sealed trait Typed[T] {
    val signature: String
  }
  object Typed {
    extension [T](t: Typed[T]) {
      def toSignature: Signature = Signature.Manual(t.signature)
    }
    inline given [T]: Typed[T] with {
      override val signature: String = Macros.signType[T]
    }
  }

  inline def auto(toSign: => Any): Signature =
    Signature.Automatic(content = Macros.sign(toSign), hash = toSign.hashCode().toHexString)

  extension (s: Signature) {

    @targetName("append")
    def +:(st: SignatureTuple): SignatureTuple = st.copy(signature = st.signature.append(s))

    // TODO: Make tailrec
    def compute(using env: Environment): String = s match {
      case a: Automatic                       => env.signatureStrategy.select(a)
      case Manual(signature)                  => signature
      case DerivedUnary(orig, derive)         => derive(orig.compute)
      case DerivedBinary(left, right, derive) => derive(left.compute, right.compute)
      case SignatureTuple(signature)          => signature.map(_.compute).toList.mkString("(", ", ", ")")
    }

    def unite(other: Signature)(derive: (String, String) => String): Signature =
      DerivedBinary(s, other, derive)

    def unite(other: WithSignature)(derive: (String, String) => String): Signature =
      unite(other.signature)(derive)

    def andThen(f: String => String): Signature = DerivedUnary(s, f)

    @targetName("add")
    def *(other: Signature): Signature     = s.unite(other)(_ + " " + _)
    @targetName("add")
    def *(other: WithSignature): Signature = s * other.signature
  }


  enum Strategy(val select: Automatic => String) {
    /**
     * Creates a signature based on a textual content of the
     * function.
     */
    case Content extends Strategy(_.content)

    /**
     * Uses hash code of a function to generate a signature
     */
    case HashCode extends Strategy(_.hash)
  }
}
