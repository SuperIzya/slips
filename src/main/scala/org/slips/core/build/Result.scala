package org.slips.core.build

import cats.data.NonEmptyList
import org.slips.core.build.Error as BuildError

sealed trait Result[+T]

object Result {
  case class Pure[T](result: T)                                          extends Result[T]
  case class WithWarnings[T](result: T, warnings: NonEmptyList[Warning]) extends Result[T]
  case class Error[T](error: BuildError)                                 extends Result[T]

  extension [T](res: Result[T]) {
    def map[Q](f: T => Q): Result[Q] = res match {
      case Result.Pure(result)                   => Result.Pure(f(result))
      case Result.WithWarnings(result, warnings) => Result.WithWarnings(f(result), warnings)
      case Result.Error(error)                   => Result.Error(error)
    }

    def flatMap[Q](f: T => Result[Q]): Result[Q] = res match {
      case Result.Pure(result)                   => f(result)
      case Result.WithWarnings(result, warnings) => f(result).withWarnings(warnings)
      case Result.Error(error)                   => Result.Error(error)
    }

    def withMaybeWarnings(newWarning: List[Warning]): Result[T] =
      NonEmptyList.fromList(newWarning).fold(res)(res.withWarnings)

    def withWarnings(newWarning: Warning, newWarnings: Warning*): Result[T] =
      res.withWarnings(NonEmptyList(newWarning, newWarnings.toList))

    def withWarnings(newWarnings: NonEmptyList[Warning]): Result[T] = res match {
      case Result.Pure(result)                   => Result.WithWarnings(result, newWarnings)
      case Result.WithWarnings(result, warnings) => Result.WithWarnings(result, warnings ++ newWarnings.toList)
      case Result.Error(error)                   => Result.Error(error)
    }

    def fold[B](onPure: T => B)(onWarnings: (T, NonEmptyList[Warning]) => B)(onError: BuildError => B): B = res match {
      case Result.Pure(result)                   => onPure(result)
      case Result.WithWarnings(result, warnings) => onWarnings(result, warnings)
      case Result.Error(error)                   => onError(error)
    }

    def toOption: Option[T] = fold(Some(_))((t, _) => Some(t))(_ => None)
  }

  def errorMsg[T](msg: String): Result[T] = error(BuildError(msg))

  def error[T](error: BuildError): Result[T] = Error(error)

  def warning[T](res: T, warning: Warning, warnings: Warning*): Result[T] =
    Result.WithWarnings(res, NonEmptyList(warning, warnings.toList))

  def result[T](res: T): Result[T] = Result.Pure(res)
}
