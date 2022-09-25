package org.slips.core

import cats.data.StateT

package object action {
  type Action[F[_], T] = StateT[F, Context[F], T]
}
