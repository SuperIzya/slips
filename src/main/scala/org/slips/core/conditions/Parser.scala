package org.slips.core.conditions

import org.slips.core.SourceLocation
import org.slips.core.conditions.Predicate.*
import org.slips.core.fact.Fact

object Parser extends PredicateSyntax {

  def apply[T](condition: Condition[T]): (Context, Fact.Val[T]) =
    parse(condition).run(Context.empty).value

  case class Context(predicates: Set[Predicate], allSources: Set[Condition.Source[?]]) {
    def addPredicate(p: Predicate): Context                = copy(predicates = predicates + p)
    def addSource[T](source: Condition.Source[T]): Context = copy(allSources = allSources + source)
  }

  object Context {
    val empty: Context = Context(Set.empty, Set.empty)
  }

  private def parse[T](condition: Condition[T]): ParseStep[T] = condition match {
    case source: Condition.Source[T]        =>
      ParseStep
        .modify(_.addSource(source))
        .map[Fact.Val[T]](_ => source.ev.flip(Fact.Source(source)(using source.T, source.ev)(using source.sourceLocation)))
    case Condition.Opaque(predicate)        => parsePredicate(predicate.toCNF)
    case Condition.Map(src, f)              => parse(src).map(f)
    case Condition.FlatMap(left, f)         => parse(left).flatMap(x => parse(f(x)))
    case filter @ Condition.Filter(cond, f) =>
      given SourceLocation = filter.sourceLocation
      for {
        t <- parse(cond)
        predicate = f(t)
        _ <- parsePredicate(predicate.toCNF)
      } yield t
  }

  private def parsePredicate(p: Predicate): ParseStep[Unit] = p match {
    case left && right =>
      for {
        _ <- parsePredicate(left)
        _ <- parsePredicate(right)
      } yield Fact.unit
    case left || right =>
      for {
        _ <- parsePredicate(left)
        _ <- parsePredicate(right)
        _ <- ParseStep.modify(_.addPredicate(p))
      } yield Fact.unit
    case !(pp)         =>
      parsePredicate(pp).flatMap(_ => ParseStep.modify(_.addPredicate(p)))
    case _             => ParseStep.modify(_.addPredicate(p))
  }
}
