package org.slips.core.conditions

import org.slips.Signature
import org.slips.core.SourceLocation
import org.slips.core.conditions.*
import org.slips.core.fact.FactOps
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.language.implicitConversions

trait PredicateSyntax {

  given Conversion[Predicate, Condition[Unit]] =
    (p: Predicate) => Condition.Opaque(p)

  extension (self: Predicate) {
    @targetName("and_op")
    inline def &&(other: Predicate): Predicate = Predicate.&&(self, other)

    @targetName("or_op")
    inline def ||(other: Predicate): Predicate = Predicate.||(self, other)

    def toCNF: Predicate = PredicateSyntax.traverseToCnf(self)
  }

  opaque type || = Predicate.||
  @targetName("or")
  object || {
    infix def unapply(p: Predicate.||): Option[(Predicate, Predicate)] = Some((p.left, p.right))
  }

  opaque type && = Predicate.&&
  @targetName("and")
  object && {
    inline infix def unapply(p: Predicate.&&): Option[(Predicate, Predicate)] = Some((p.left, p.right))
  }

  extension [T](self: Predicate.Test[T]) {
    def signed(signature: => String): Predicate.Test[T] =
      signed(Signature.Manual(signature))

    def signed(signature: Signature): Predicate.Test[T] =
      self.copy(signature = signature)(using self.T, self.sourceLocation)
  }
}

object PredicateSyntax {

  private object TreeCrawler {

    import Predicate.*

    inline def treeEnd(p: Predicate, tasks: List[NextTask], results: List[Predicate]): Predicate = {
      if tasks.isEmpty then p
      else processTasks(tasks.head, tasks.tail, p :: results)
    }

    @tailrec
    private def processTasks(task: NextTask, tasks: List[NextTask], results: List[Predicate]): Predicate =
      task match {
        case TraverseToCnf(p) =>
          traverseToCnf(p, tasks, results)
        case Compute(f)       =>
          f(results, tasks) match {
            case ComputationResult(p, results)    => treeEnd(p, tasks, results)
            case NextComputations(results, tasks) => processTasks(tasks.head, tasks.tail, results)
          }
      }

    def computeAndN(n: Int): NextTask = Compute { (results, _) =>
      ComputationResult(results.take(n).reduce(_ && _), results.drop(n))
    }

    val computeAnd: NextTask = computeAndN(2)

    val computeOr: Compute = Compute { (results, tasks) =>
      def andResult(rs: Seq[Predicate], a: &&, other: Predicate): NextComputations = NextComputations(
        rs.toList,
        TraverseToCnf(a.left || other) :: TraverseToCnf(a.right || other) :: computeAnd :: tasks
      )

      (results: @unchecked) match {
        case List(l, r, rs*) =>
          (l, r) match {
            case (l1 && r1, l2 && r2) =>
              NextComputations(
                rs.toList,
                TraverseToCnf(l1 || l2) ::
                  TraverseToCnf(r1 || r2) ::
                  TraverseToCnf(l1 || r2) ::
                  TraverseToCnf(r1 || l2) ::
                  computeAndN(4) ::
                  tasks
              )
            case (a: &&, right)       =>
              andResult(rs, a, right)
            case (left, a: &&)        =>
              andResult(rs, a, left)
            case (left, right)        =>
              ComputationResult(left || right, rs.toList)
          }
      }
    }
  }
  @tailrec
  private[PredicateSyntax] def traverseToCnf(
    p: Predicate,
    tasks: List[NextTask] = Nil,
    results: List[Predicate] = Nil
  ): Predicate = {
    import Predicate.*
    import TreeCrawler.*

    p match {
      case t: (Test[?] | Exist[?] | NotExist[?]) =>
        treeEnd(t, tasks, results)
      case !(p)                                  =>
        p match {
          case left && right   =>
            traverseToCnf(!left || !right, tasks, results)
          case left || right   =>
            val newTasks = TraverseToCnf(!right) :: computeAnd :: tasks
            traverseToCnf(!left, newTasks, results)
          case t: Test[?]      => treeEnd(t.not, tasks, results)
          case !(p)            => traverseToCnf(p, tasks, results)
          case ex: Exist[?]    => treeEnd(ex.not, tasks, results)
          case ex: NotExist[?] => treeEnd(ex.not, tasks, results)
        }
      case left && right                         =>
        traverseToCnf(left, TraverseToCnf(right) :: computeAnd :: tasks, results)
      case left || right                         =>
        inline def process(first: Predicate, rest: Predicate*): Predicate = {
          val newTasks = rest.map(TraverseToCnf(_)).toList ++ List(computeAndN(rest.length + 1)) ++ tasks
          traverseToCnf(first, newTasks, results)
        }
        (left, right) match {
          case (l1 && r1, l2 && r2) =>
            process(l1 || l2, r1 || r2, l1 || r2, r1 || l2)
          case (l && r, right)      =>
            process(l || right, r || right)
          case (left, l && r)       =>
            process(left || l, left || r)
          case (left, right)        =>
            traverseToCnf(left, TraverseToCnf(right) :: computeOr :: tasks, results)
        }

    }
  }

  private sealed trait NextTask

  private case class TraverseToCnf(p: Predicate) extends NextTask

  private case class Compute(f: (List[Predicate], List[NextTask]) => ComputationResult | NextComputations)
      extends NextTask

  private case class NextComputations(results: List[Predicate], tasks: List[NextTask])

  private case class ComputationResult(predicate: Predicate, results: List[Predicate])
}
