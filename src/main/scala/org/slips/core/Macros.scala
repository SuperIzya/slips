package org.slips.core

import scala.quoted.*

object Macros {
  inline def createSigned[R](inline res: String => R, toSign: Any): R =
    ${ createSignedImp('res, 'toSign) }

  def createSignedImp[R: Type](res: Expr[String => R], toSign: Expr[Any])(using quotes: Quotes): Expr[R] = {
    import quotes.*
    import reflect.*
    val signature: Expr[String] = {
      val signs: String = toSign.asTerm.show
      val sign = s"[${Type.show[R]}]($signs)"
      println(sign)
      Expr(sign)
    }

    '{ $res($signature) }
  }

}
