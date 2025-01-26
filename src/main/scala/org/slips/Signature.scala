package org.slips

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.kernel.Order
import org.slips.core.WithSignature
import org.slips.core.macros.Macros
import scala.annotation.targetName

sealed trait Signature { self =>
  def append(postfix: String): Signature = Signature.DerivedUnary(self, _ + postfix)
  def prepend(prefix: String): Signature = Signature.DerivedUnary(self, s => prefix + s)
}

object Signature {
  type TupleSign = NonEmptyList[Signature]
  object TupleSign {
    def first(sign: Signature): TupleSign = NonEmptyList.one(sign)
  }

  case class Automatic private[Signature] (hash: String, content: String) extends Signature
  case class Manual(signature: String)                                    extends Signature

  case class DerivedUnary(orig: Signature, derive: String => String)                              extends Signature
  case class DerivedBinary(left: Signature, right: Signature, derive: (String, String) => String) extends Signature

  case class SignatureTuple(signature: TupleSign) extends Signature
  object SignatureTuple {
    def apply(s: Signature): SignatureTuple = SignatureTuple(TupleSign.first(s))

  }

  inline def auto(inline toSign: => Any): Signature =
    Signature.Automatic(content = Macros.sign(toSign), hash = toSign.hashCode().toHexString)

  def hash(toSign: => Any): Signature               = Signature.Manual(toSign.hashCode().toHexString)
  inline def sign(inline toSign: => Any): Signature = Signature.Manual(Macros.sign(toSign))

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
      * Creates a signature based on a textual content of
      * the function.
      */
    case Content extends Strategy(_.content)

    /** Uses hash code of a function to generate a signature */
    case HashCode extends Strategy(_.hash)
  }

  sealed trait SignType[T] extends WithSignature
  object SignType {
    case class TypeSignature[T](single: Signature) extends SignType[T] {
      override def signature: Signature = single
    }
    case class TupleSignature[T](tuple: TupleSign) extends SignType[T] {
      override def signature: Signature = Signature.SignatureTuple(tuple)
    }

    inline given nonTuple: [T: NotTuple] => SignType.TypeSignature[T] = SignType.TypeSignature(Manual(Macros.signType[T]))

    given tuple: [H : {NotTuple, TypeSignature as H}, T <: NonEmptyTuple : {TupleSignature as T}]
      => TupleSignature[H *: T] =
      TupleSignature(T.tuple.prepend(H.single))

    given firstTuple: [H : {NotTuple, TypeSignature as H}] => TupleSignature[H *: EmptyTuple] =
      TupleSignature(TupleSign.first(H.single))

  }
}
