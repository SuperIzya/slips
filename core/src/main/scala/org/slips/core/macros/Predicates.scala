package org.slips.core.macros

import org.slips.core.Condition.UsedSymbols
import org.slips.core.{Condition, HasFact}

import scala.quoted.{Expr, Quotes, Type}

object Predicates {
  private def parse(using quotes: Quotes)(expr: Expr[Boolean]): UsedSymbols = {
    import quotes.reflect._
    import scala.language.postfixOps
    /*def run(t: Tree, symbols: Map[String, String]): Map[String, String] = {
      println(s"+++${t.symbol}")
      t match {
        case Inlined(treeM, definitions, term) => treeM.fold{
          println(s"Inlined end of the line $definitions, ${term.symbol}")
          run(term, symbols)
        } { t =>
          println("Inlined")
          run(term, run(t, symbols))
        }
        case Apply(f: Term, args: List[Term]) =>
          println(s"function ${f.symbol}")

          val runF = run(f, symbols)
          println(s"RunF: $runF")

          args.foldLeft(runF)((a, s) => run(s, a))
         //println(s"function ${f.show} on memeber ${y.show} of ${x.show} of type")

        case Select(t, _) => run(t, symbols)

        case Ident(symbol) =>
          val res: Set[String] = symbols + (symbol -> t.
          println(s">> $res")
          res

        case Lambda(vd, t) =>
          println(s"lambda with $vd and $t")
          symbols

        case ByName(t: TypeTree) =>
          println(s"")
          symbols

        case _ =>
          println("smth else")
          symbols
      }
    }*/

    def runTerm(t: Term, symbols: UsedSymbols): UsedSymbols = {
      t match {
        case Select(a, b) =>
          println(s"tttt  '${a.symbol}'  tttt")
          println(s"sssss $b sssss")
          runTerm(a, symbols)
        case Ident(x) => symbols + x
        case _ => symbols
      }
    }
    def run(ev: Expr[Any], symbols: UsedSymbols): UsedSymbols = {
      println(ev.show)
      ev match {
        case '{
      type t
      ($a: `t`) != ($b: `t`)
      } => run(b, run(a, symbols))
        case '{ $a: p } =>
          println(s"++++ ${Type.show[p]} ")
          runTerm(a.asTerm, symbols)
        case _ => symbols
      }
    }

    val res = run(Expr.betaReduce(expr), UsedSymbols.empty)
    println()
    println()
    println(res)
    println()
    res
  }

  def singleFact[A](value: Expr[Boolean])(using Type[A], Quotes): Expr[Condition[HasFact[A], A]] = {

    val usedSymbols: Expr[UsedSymbols] = Expr.apply(parse(value))
    val dummy: Expr[Condition[HasFact[A], A]] = '{
      Condition.Dummy[HasFact[A], A](_ => ${value})
    }

    val dummies: Expr[List[Condition[HasFact[A], A]]] = Expr.ofList(
      List(dummy)
    )
    println(value)
    '{
      Condition.Temp($dummies, $usedSymbols)
    }
  }

}
