package org.slips.core.action

import org.slips.EnvAction
import org.slips.Environment
import org.slips.NotTuple
import org.slips.core.action.FactId.Val

trait FactIdOps[T] {

  def valueAction(f: FactId.Val[T]): (env: Environment) ?=> env.Action[T]
}

object FactIdOps {

  given factIdOpsEmptyTuple[H](
    using H: FactIdOps[H],
    ev: FactId[H] =:= FactId.Val[H]
  ): FactIdOps[H *: EmptyTuple] with {
    override def valueAction(f: FactId.Val[H *: EmptyTuple]): EnvAction[H *: EmptyTuple] = env ?=>
      import Environment.Syntax.Actions.*
      val head *: _ = f
      for {
        h <- H.valueAction(ev(head))
      } yield h *: EmptyTuple
  }

  given factIdOpsTupleStep[T <: NonEmptyTuple, H](
    using T: FactIdOps[T],
    H: FactIdOps[H],
    ev: FactId[H] =:= FactId.Val[H]
  ): FactIdOps[H *: T] with {
    override def valueAction(f: FactId.Val[H *: T]): EnvAction[H *: T] = env ?=> {
      import Environment.Syntax.Actions.*
      val head *: tail = f
      for {
        h <- H.valueAction(ev(head))
        t <- T.valueAction(tail)
      } yield h *: t
    }
  }

  given factIdOpsScalar[T](using ev: FactId[T] =:= FactId.Val[T]): FactIdOps[T] with {
    override def valueAction(f: FactId.Val[T]): EnvAction[T] = env ?=> {
      import Environment.Syntax.Actions.*
      ev.flip(f).value
    }
  }
}
