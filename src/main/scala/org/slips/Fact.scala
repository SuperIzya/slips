package org.slips

import org.slips.core.Quantor

case class Fact[+T](origin: Quantor[T])
