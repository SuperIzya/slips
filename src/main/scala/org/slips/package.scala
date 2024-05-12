package org

import scala.util.NotGiven

package object slips {
  type Env[T]      = Environment ?=> T
  type NotTuple[T] = NotGiven[T <:< Tuple]
}
