package org.slips.syntax

import org.slips.data.Data

object TestManySyntaxTest extends FactSyntax {
  private val parse2 = for {
    l <- all[Data.Leaf]
    b <- all[Data.Berry]
    _ <- ((l, b)).testM { p => p._1.name.notEmpty || p._2.name.notEmpty }
  } yield ()

}
