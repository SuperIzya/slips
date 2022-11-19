package org.slips.core

package object network {
  type Source[T] = T match
    case Tuple => Tuple.Map[T, Source]
    case _     => Node
}
