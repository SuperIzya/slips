package org.slips

import org.scalatest.flatspec.AnyFlatSpec
import org.slips.Rule.Aux
import org.slips.core.Condition.{All, Not}
import org.slips.core.{Condition, HasFact, Test}

class SyntacticTest extends AnyFlatSpec {

  case class Color(color: String)
  case class Shape(shape: String)

  val rule: Rule[Unit] = Rule("test") {
    for {
      a <- All[Color]
      b <- All[Shape]
      d = a
      c <- Test(d.color != "")
    } yield (c, b)
  }{ case (a, b) => _ => {
    //println(a.color)
    println(b.shape)
  } }
/*
  val rule2: Rule[Unit] = Rule("test2") {
    for {
      a <- Not(All[Color])
      b <- Not(All[Color])
      /*_ <- Test(a.color != "")
      _ <- Test(a.color != b.color)
      _ <- Test(a.color != "a" && b.color != "b")*/
    } yield (a, b)
  }{ (a, b) => _ => {
    println(a.color)
    println(b.color)
  } }*/

  println(rule.condition)

  val ruleSet: RuleSet[Unit] = RuleSet(rule)//, rule2)
}
