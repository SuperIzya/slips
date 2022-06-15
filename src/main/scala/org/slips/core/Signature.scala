package org.slips.core

import scala.util.NotGiven

trait Signature[T <: Tuple]:
  def extract(r: Tuple.Map[T, Signature.SignedT]): Signature.TupleSignature

object Signature {
  type SignedT = [x] =>> Signed
  type TupleSignature = List[String]
  inline def apply[T <: Tuple: Signature]: Signature[T] = summon[Signature[T]]

  case class Empty[T <: Tuple]() extends Signature[T] {
    override def extract(r: Tuple.Map[T, SignedT]): TupleSignature = List.empty
  }
  object Empty:
    def apply[T <: Tuple](using DummyImplicit): Signature[T] = new Empty()

  given Signature[EmptyTuple] = Empty()


  given[H, T <: Tuple](using ev: NotGiven[H <:< Tuple], s: Signature[T]): Signature[H *: T] with
    override def extract(r: Tuple.Map[H *: T, SignedT]): TupleSignature = r match
      case head *: tail => head.signature +: s.extract(tail)
}

