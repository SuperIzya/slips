package org.slips.macros

import org.slips.Fact
import org.slips.core.Quantor
import org.slips.core.Quantor.All
import org.slips.syntax.*


import scala.annotation.tailrec
import scala.quoted.*

object ConditionBuilder {
  inline def apply[T](inline expr: Expression[T]): Condition[T] = ${ transform('expr) }

  def transform[T: Type](expr: Expr[Expression[T]])(using Quotes): Expr[Condition[T]] = {
    new Builder(expr, quantors = '{ EmptyTuple }).transform
  }

  private class Builder[T: Type](
      expr: Expr[Expression[T]],
      quantors: Expr[Tuple],
      tests: List[Expr[Boolean]] = List.empty,
      vars: List[String] = List.empty
  )(using quotes: Quotes, E: Type[Expression[T]]) {
    import quoted.quotes.*
    import quotes.reflect.*

    @tailrec
    final private def extractName(t: Any): String = t match {
      case List(next) => extractName(next)
      case ValDef(name, _, _) => name
    }

    @tailrec
    private def getBodyBlock(t: Any, possibleName: Option[String] = None): (String, Expr[Expression[T]]) = {
      t match {
        case List(next) => getBodyBlock(next)
        case Match(_, List(caseDef)) =>
          getBodyBlock(caseDef, possibleName)
        case CaseDef(f, s, t) =>
          Printer.TreeStructure.show(f)
          s.fold(println("None"))(Printer.TreeStructure.show)
          Printer.TreeStructure.show(t)
          ???
        case DefDef(_, name, _, Some(body)) => getBodyBlock(body, Some(extractName(name)))
        case x: Term => (possibleName.get, x.asExprOf[Expression[T]])
        case _ =>
          ???
      }
    }

    @tailrec
    private def getBody(t: Any): (String, Expr[Expression[T]]) = {
      println(
        s"""body:
           |$t
           |
           |""".stripMargin)
      t match {
        case List(next) => getBodyBlock(next)
        case Block(Nil, next) => getBody(next)
        case Block(next, _) => getBody(next)
        case _ => getBodyBlock(t)
      }
    }

    def transform: Expr[Condition[T]] = {
      println()
      println(expr.show)
      expr match {
        case '{ ($quantor: Quantor[T]) } => '{ Facts.single($quantor).map(_._1) }
        case '{ ($test: Boolean).flatMap($flatMap: Boolean => Expression[T]) } =>
          val (_, body) = getBody(flatMap.asTerm)
          new Builder(
            expr = body,
            quantors = quantors,
            tests = buildCnf(test) +: tests,
            vars = vars
          ).transform
        case '{ type t; ($quantor: Quantor[`t`]).flatMap($flatMap: `t` => Expression[T]) } =>
          quantor match {
            case '{ $q: Quantor[t] } =>
                val (v, body) = getBody(flatMap.asTerm)
                new Builder(
                  expr = body,
                  quantors = '{ $quantor *: $quantors },
                  tests = tests,
                  vars = v +: vars
                ).transform
          }
        case _ => ???
      }

    }


    def buildCnf(bExpr: Expr[Boolean]): Expr[Boolean] = {
      def parse(e: Expr[Boolean]): Expr[Boolean] = {
        e match {
          case '{ (($e1: Boolean) && ($e2: Boolean)) || (($e3: Boolean) && ($e4: Boolean)) } =>
            val p1 = parse(e1)
            val p2 = parse(e2)
            val p3 = parse(e3)
            val p4 = parse(e4)
            val a1 = parse('{ $p1 || $p3 })
            val a2 = parse('{ $p2 || $p3 })
            val a3 = parse('{ $p1 || $p4 })
            val a4 = parse('{ $p2 || $p4 })
            '{ ($a1) && ($a2) && ($a3) && ($a4) }
          case '{ (($e1: Boolean) && ($e2: Boolean)) || ($e3: Boolean) } =>
            val p1 = parse(e1)
            val p2 = parse(e2)
            val p3 = parse(e3)
            val a1 = parse('{ $p1 || $p3 })
            val a2 = parse('{ $p2 || $p3 })
            '{ ($a1) && ($a2) }
          case '{ ($e1: Boolean) || (($e2: Boolean) && ($e3: Boolean)) } =>
            val p1 = parse(e1)
            val p2 = parse(e2)
            val p3 = parse(e3)
            val a1 = parse('{ $p1 || $p2 })
            val a2 = parse('{ $p1 || $p3 })
            '{ ($a1) && ($a2) }
          case '{ !(($e1: Boolean) && ($e2: Boolean)) } =>
            val p1 = parse('{ !$e1 })
            val p2 = parse('{ !$e2 })
            parse('{ $p1 || $p2 })
          case '{ !(($e1: Boolean) || ($e2: Boolean)) } =>
            val p1 = parse('{ !$e1 })
            val p2 = parse('{ !$e2 })
            '{ $p1 && $p2 }
          case '{ ($e1: Boolean) && ($e2: Boolean) } => '{ ${ parse(e1) } && ${ parse(e2) } }
          case '{ ($e1: Boolean) && ($e2: Boolean) && ($e3: Boolean) } =>
            val p1 = parse(e1)
            val p2 = parse(e2)
            val p3 = parse(e3)
            val a1 = parse('{ $p1 && $p2 })
            parse('{ ($a1) && $p3 })
          case '{ ($e1: Boolean) || ($e2: Boolean) || ($e3: Boolean) } =>
            val p1 = parse(e1)
            val p2 = parse(e2)
            val p3 = parse(e3)
            val a1 = parse('{ $p1 || $p2 })
            parse('{ ($a1) || $p3 })
          case '{ ($e1: Boolean) || ($e2: Boolean) && ($e3: Boolean) } =>
            val p1 = parse(e1)
            val p2 = parse(e2)
            val p3 = parse(e3)
            val a1 = parse('{ $p1 || $p2 })
            parse('{ ($a1) && $p3 })
          case '{ ($e1: Boolean) && ($e2: Boolean) || ($e3: Boolean) } =>
            val p1 = parse(e1)
            val p2 = parse(e2)
            val p3 = parse(e3)
            val a1 = parse('{ $p2 || $p3 })
            parse('{ $p1 && ($a1)  })

          case _ => e
        }
      }
      parse(bExpr)
    }
  }
}
