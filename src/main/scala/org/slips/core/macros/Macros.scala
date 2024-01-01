package org.slips.core.macros

import scala.quoted.*
import scala.util.matching.Regex

object Macros {
  inline def sign(inline toSign: Any): String = ${ signAnyImpl('toSign) }
  inline def signType[T]: String              = ${ signTypeImpl[T] }

  private def signAnyImpl(toSign: Expr[Any])(using Quotes): Expr[String] = {
    Expr(signAny(toSign))
  }

  private def cleanupSignature(in: String): String = {
    val re = """(_\$\d+)""".r

    extension (s: String)
      def replaceWithChar(what: String, substitute: String)(char: Char): String =
        s.replace(what + char, substitute + char)

    re.findAllMatchIn(in)
      .map(_.group(0))
      .toList
      .distinct
      .zipWithIndex
      .foldLeft(in) { (res, x) =>
        x match {
          case (v, i) =>
            res
              .replace(s"$v: ", "")
              .replace(s"$v:", "")
              .replaceWithChar(v, s"_$$$i")('.')
              .replaceWithChar(v, s"_$$$i")(')')
              .replaceWithChar(v, s"_$$$i")('-')
              .replaceWithChar(v, s"_$$$i")('(')
              .replaceWithChar(v, s"_$$$i")(',')
              .replaceWithChar(v, s"_$$$i")(' ')
        }
      }
  }

  private def signAny(toSign: Expr[Any])(using q: Quotes): String = {
    import q.*
    import reflect.*

    val signs: String = toSign
      .asTerm
      .show(using Printer.TreeCode)
    cleanupSignature(signs)
  }

  private def signTypeImpl[T: Type](using Quotes): Expr[String] = {
    val signed = Type.show[T]
    Expr(signed)
  }
}
