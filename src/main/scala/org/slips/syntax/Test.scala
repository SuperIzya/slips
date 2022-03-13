package org.slips.syntax

import org.slips.Fact

sealed trait Test {
  def and(c: Test): Test = ???
  def or(c: Test): Test  = ???
  def not(c: Test): Test = ???
}
object Test {
  def test[T](fact: Fact[T])(cond: T => Boolean): Test                                 = ???
  def test[T, Q](f1: Fact[T], f2: Fact[Q])(cond: (T, Q) => Boolean): Test              = ???
  def test[F <: Tuple, T <: Tuple.InverseMap[F, Fact]](f: F)(cond: T => Boolean): Test = ???

}
