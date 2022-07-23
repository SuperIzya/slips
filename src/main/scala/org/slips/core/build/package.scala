package org.slips.core

import cats.data.State

package object build {

  type BuildStep[x] = State[BuildContext, x]
}
