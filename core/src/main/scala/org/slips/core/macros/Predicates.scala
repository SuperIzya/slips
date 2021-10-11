package org.slips.core.macros

import org.slips.core.{Condition, HasFact}

import scala.quoted.{Expr, Quotes, Type}

object Predicates {
  private def parse(using quotes: Quotes)(expr: Expr[Boolean]): Set[String] = {
    import quotes.reflect._
    import scala.language.postfixOps
    def run(t: Tree, symbols: Set[String]): Set[String] = {
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
          println(s"function ${f.asExprOf[Any].show}")

          val runF = run(f, symbols)
          println(s"RunF: $runF")

          args.foldLeft(runF)((a, s) => run(s, a))
         //println(s"function ${f.show} on memeber ${y.show} of ${x.show} of type")

        case Select(t, _) => run(t, symbols)

        case Ident(symbol) =>
          val res: Set[String] = symbols + symbol
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
    }
    val res = run(Expr.betaReduce(expr).asTerm, Set.empty)
    println()
    println()
    println(res)
    println()
    res
  }

  def singleFact[A](value: Expr[Boolean])(using Type[A], Quotes): Expr[Condition[HasFact[A], A]] = {

    val usedSymbols: Expr[Set[String]] = Expr.apply(parse(value))
    val dummy: Expr[Condition[HasFact[A], A]] = '{
      Condition.Dummy[HasFact[A], A](_ => ${value})
    }

    println(dummy.show)

    val dummies: Expr[List[Condition[HasFact[A], A]]] = Expr.ofList(
      List(dummy)
    )
    println(value)
    '{
      Condition.Temp($dummies).usingSymbols($usedSymbols)
    }
  }

}
