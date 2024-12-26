package org.slips.core.build

import cats.data.NonEmptyList
import zio.internal.stacktracer.SourceLocation
import zio.test.{ErrorMessage as M, Result as _, *}
import zio.test.Assertion.anything

object ResultAssertion {
  val isError: Assertion[Result[Any]]  = error(anything)
  val notError: Assertion[Result[Any]] = result(anything)

  def error[T](a: Assertion[Error])(using SourceLocation): Assertion[Result[T]] = {
    val head                                   = M.text("Result error assertion") + M.choice("passed", "failed")
    def fail(a: Result[T]): TestTrace[Nothing] = TestTrace.fail(head + M.value(a) + M.text("is not an error"))
    Assertion(
      TestArrow.make[Result[T], Boolean] {
        case a @ Result.Pure(_)            => fail(a)
        case a @ Result.WithWarnings(_, _) => fail(a)
        case Result.Error(error)           =>
          TestTrace.boolean(a.test(error)) { head }
      }
    )
  }

  def result[T](a: Assertion[T])(using SourceLocation): Assertion[Result[T]] = {
    def head = M.text("Result value assertion") + M.choice("passed", "failed")
    def assertVal(t: T, maybeWarnings: Option[NonEmptyList[Warning]]) = {
      val res = a.test(t)
      TestTrace.boolean(res) {
        head + maybeWarnings.fold(M.text("")) { warnings =>
          warnings.foldLeft(M.text("with warnings:"))((t, w) => t + M.custom(w.message))
        }
      }
    }
    Assertion(
      TestArrow.make[Result[T], Boolean] {
        case Result.Pure(result)                   => assertVal(result, None)
        case Result.WithWarnings(result, warnings) => assertVal(result, Some(warnings))
        case Result.Error(error) => TestTrace.fail(head + M.text("with error:") + M.custom(error.message))
      }
    )
  }
  extension [T](res: Result[T]) {
    def assertField[Q](name: String, selector: T => Q, assert: Assertion[Q]): Assertion[Result[T]] =
      result(Assertion.hasField(name, selector, assert))
  }
}
