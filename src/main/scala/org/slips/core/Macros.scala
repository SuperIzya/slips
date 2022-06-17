package org.slips.core

import cats.data.State

import scala.quoted.*
import scala.util.matching.Regex

object Macros {
  inline def createSigned[R]( res: String => R, inline toSign: Any): R = ${ createSignedImp('res, 'toSign) }

  def createSignedImp[R: Type](res: Expr[String => R], toSign: Expr[Any])(using quotes: Quotes): Expr[R] = {
    import quotes.*
    import reflect.*

    def cleanupSignature(in: String): String = {
      val re = """(_\$\d+)""".r

      extension (s: String) def escape(c: Char): String = s.replace("" + c, s"\\" + c)

      val toRe: String => String = _.escape('_')
        .escape('.')
        .escape(':')

      re.findAllMatchIn(in).map(_.group(0)).toList.distinct.zipWithIndex
        .foldLeft(in) { (res, x) =>
          x match {
            case (v, i) =>
              val n = res
                .replace(s"$v:", "")
                .replace(s"$v.", s"_$$$i.")
                .replace(s"$v)", s"_$$$i)")

              n + " "
          }
        }
    }

    val signature: Expr[String] = {
      val signs: String = toSign.asTerm.show(using Printer.TreeCode)
      val sign = cleanupSignature(s"${Type.show[R]}{$signs}")
      Expr(sign)
    }

    '{ $res($signature) }
  }
  
  inline def signType[T]: String = ${ signTypeImpl[T] }

  def signTypeImpl[T: Type](using Quotes): Expr[String] = {
    val signed = Type.show[T]
    Expr(signed)
  }
}
