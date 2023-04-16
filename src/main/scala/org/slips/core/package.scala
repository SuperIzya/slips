package org.slips

package object core {
  type Signed[T] = SignatureStrategy ?=> T
}
