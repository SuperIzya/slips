package org.slips.core.network.alpha

import cats.data.State
import cats.syntax.all.*
import org.slips.core.fact.Fact
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

private[network] case class FactsFolder(
  // TODO: Make sure nodes are sorted from the largest to the smallest set.
  unionNodes: SortedMap[Set[Chain], Chain.Combine] = SortedMap.empty,
  facts: Map[Fact.Alpha[?], FactProgress] = Map.empty,
  origin: Map[Fact.Alpha[?], Set[Chain]] = Map.empty
) {

  /**
    * Adds or replaces [[FactProgress]] in
    * [[FactsFolder.facts]]
    */
  def setFactProgress(fp: FactProgress): FactsFolder = copy(facts = facts + (fp.fact -> fp))

  /**
    * Sets [[FactProgress]] for [[head]] to either
    * [[FactProgress.InProgress]] if [[left]] is not empty,
    * [[FactProgress.Done]] otherwise.
    */
  def setFactProgress(tp: ToProcess, left: Set[Chain], topChain: Chain): FactsFolder = setFactProgress {
    if (left.isEmpty) FactProgress.Done(tp.fact, tp.chains, topChain)
    else
      FactProgress.InProgress(tp.fact, tp.chains, tp.chains -- left, left, topChain)
  }

  def addUnion(chains: Set[Chain], left: Chain, right: Chain): (FactsFolder, Chain.Combine) = {
    unionNodes
      .get(chains)
      .map(this -> _)
      .getOrElse {
        val combine = Chain.Combine(left, right)
        copy(unionNodes = unionNodes + (chains -> combine)) -> combine
      }
  }

}

private[network] object FactsFolder {

  import FactProgress.*

  /** Adds  last chain for a fact. */
  private def addImmediate1(fact: Fact.Alpha[?], chain: Chain): FoldState[Unit] = for {
    ff   <- FoldState.get
    done <- ff
      .facts
      .get(fact)
      .map {
        case InProgress(_, totalChains, _, _, topChain) =>
          FoldState(_.addUnion(totalChains, topChain, chain)).map { Done(fact, totalChains, _) }
        case d: Done if d.chains.contains(chain)        => FoldState.pure(d)
      }
      .getOrElse(FoldState.pure {
        Done(fact, Set(chain), chain)
      })
    _    <- FoldState.modify(_.setFactProgress(done))
  } yield ()

  /**
    * Adds last two chains for a fact.
    *   1. Adds [[AlphaNode.Combine]] for two last
    *      [[Chain]].
    *   1. progress for the fact
    *      - Some([[FactProgress.InProgress]]) - combine
    *        result from previous step and topNode and save
    *        it in [[FactProgress.Done]]
    *      - Some([[FactProgress.Done]]) - How can that be?
    *        TODO: Tests! More reasoning!
    *      - None - make [[FactProgress.Done]] with combine
    *        from previous step.
    */
  private def addImmediate2(fact: Fact.Alpha[?], chains: Set[Chain]): FoldState[Unit] = for {
    union    <- FoldState(_.addUnion(chains, chains.head, chains.last))
    ff       <- FoldState.get
    progress <- ff.facts.get(fact) match {
      case Some(InProgress(_, totalChains, _, _, topNode)) =>
        FoldState(_.addUnion(totalChains, union, topNode)).map {
          Done(fact, chains, _)
        }

      case Some(d: Done) if chains.subsetOf(d.chains) => FoldState.pure(d)

      case None => FoldState.pure(Done(fact, chains, union))
    }
    _        <- FoldState.modify(_.setFactProgress(progress))
  } yield ()
  given Ordering[Set[Chain]] = Ordering.fromLessThan((x, y) => x.size > y.size)

  /**
    * Finds first element of `nodes` that key is subset of
    * `supSet`
    * @param nodes
    *   [[SortedMap]] of chains combined by
    *   [[AlphaNode.Combine]].
    * @param supSet
    *   super set of chains to be chopped off later. The
    *   bigger subset found on this step - the better
    * @return
    *   for given `supSet` of [[Chain]] the biggest possible
    *   subset with already combined chains.
    */
  @tailrec
  private def findSubset(
    nodes: SortedMap[Set[Chain], Chain.Combine],
    supSet: Set[Chain]
  ): Option[(Set[Chain], Chain.Combine)] = {
    if (nodes.isEmpty) None
    else {
      val head = nodes.head
      if (head._1.subsetOf(supSet)) Some(head)
      else findSubset(nodes.tail, supSet)
    }
  }

  private case class Solution(left: Chain, right: Chain, intersection: Set[ToProcess])

  private object Solution {
    def apply(example: (Chain, Set[ToProcess]), toCompare: (Chain, Set[ToProcess])): Option[Solution] = {
      val intersection = example._2.intersect(toCompare._2)
      Option.when(intersection.nonEmpty)(Solution(example._1, toCompare._1, intersection))
    }

    @tailrec
    private def findLocalSolution(
      matchAgainst: (Chain, Set[ToProcess]),
      toCompare: SortedSet[(Chain, Set[ToProcess])],
      solutionM: Option[Solution] = None
    ): Option[Solution] = {
      if (toCompare.isEmpty) solutionM
      else {
        val head = toCompare.head
        if (solutionM.isDefined) {
          val solution = solutionM.get
          if (solution.intersection.size >= head._2.size) Some(solution)
          else {
            val next = Solution(matchAgainst, head)
              .filter {
                _.intersection.size > solution.intersection.size
              }
              .getOrElse(solution)

            findLocalSolution(matchAgainst, toCompare.tail, Some(next))
          }
        } else {
          findLocalSolution(matchAgainst, toCompare.tail, Solution(matchAgainst, head))
        }
      }
    }

    @tailrec
    def selectPair(
      toCompare: SortedSet[(Chain, Set[ToProcess])],
      solutionM: Option[Solution] = None
    ): Option[Solution] = {
      if (toCompare.isEmpty) solutionM
      else if (toCompare.size == 1) solutionM
      else {
        val head         = toCompare.head
        val nextSolution = findLocalSolution(head, toCompare.tail, solutionM)
        val res          = (nextSolution, toCompare.tail.headOption)
          .mapN { (solution, next) => Option.when(solution.intersection.size >= next._2.size)(solution) }
          .flatten

        if (res.isDefined) res
        else selectPair(toCompare.tail, nextSolution)
      }
    }
  }
  private def suggestPair(toProcess: SortedSet[ToProcess]): (Chain, Chain) = {
    if (toProcess.size == 1) {
      // 3 or more unconnected chains for the last one
      val chains = toProcess.head.chains
      (chains.head, chains.last)
    } else {
      given Ordering[(Chain, Set[ToProcess])] = Ordering.fromLessThan((x, y) => x._2.size >= y._2.size)

      val allChains: SortedSet[(Chain, Set[ToProcess])] = SortedSet.from {
        toProcess
          .iterator
          .flatMap(p => p.chains.map(_ -> p))
          .toSeq
          .groupBy(_._1)
          .view
          .mapValues(_.map(_._2).toSet)
          .toList
      }

      Solution
        .selectPair(allChains)
        .map(s => s.right -> s.left)
        .getOrElse(toProcess.last.chains.head -> toProcess.last.chains.last)
    }
  }
  private def onSubset(progressM: Option[FactProgress], head: ToProcess)(
    subset: (Set[Chain], Chain.Combine)
  ): FoldState[Set[Chain]] = {
    progressM match {
      case None                                                   =>
        val left = head.chains -- subset._1
        FoldState.modify { _.setFactProgress(head, left, subset._2) }.map(_ => left)
      case Some(InProgress(fact, chains, united, left, topChain)) =>
        for {
          combine <- FoldState(_.addUnion(united ++ subset._1, topChain, subset._2))
          newLeft = left -- subset._1
          _ <- FoldState.modify(_.setFactProgress(head, newLeft, combine))
        } yield newLeft

    }
  }

  // TODO: Make sure the order is from the smallest unprocessed to the largest
  def fold(toProcess: SortedSet[ToProcess]): FoldState[Unit]                   = {
    if (toProcess.isEmpty) FoldState.pure(())
    else {
      val head = toProcess.head
      if (head.chains.size == 1) addImmediate1(head.fact, head.chains.head).flatMap(_ => fold(toProcess.tail))
      else if (head.chains.size == 2) addImmediate2(head.fact, head.chains).flatMap(_ => fold(toProcess.tail))
      else
        for {
          fs <- FoldState.get
          _  <- findSubset(fs.unionNodes, head.chains)
            .map {
              onSubset(fs.facts.get(head.fact), head)
            }
            .map(_.flatMap { left =>
              if (left.nonEmpty) fold(toProcess.tail + ToProcess(head.fact, left))
              else fold(toProcess.tail)
            })
            .getOrElse {
              val (left, right) = suggestPair(toProcess)
              FoldState(_.addUnion(Set(left, right), left, right)).flatMap(_ => fold(toProcess))
            }
        } yield ()
    }
  }
  def apply(origin: Map[Fact.Alpha[?], Set[Chain]]): Map[Fact.Alpha[?], Chain] = {

    fold(SortedSet.from(origin.view.iterator.map { case (fact, chains) => ToProcess(fact, chains) }))
      .runS(new FactsFolder(origin = origin))
      .map(_.facts.view.mapValues(_.topChain).toMap)
      .value
  }
}
