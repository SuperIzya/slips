package org.slips

import org.slips.core.Quantor

package object syntax {
  extension(b: Boolean) {
    def map[T](f: Boolean => T): Expression[T]                 = ???
    def flatMap[T](f: Boolean => Expression[T]): Expression[T] = ???
  }
}
