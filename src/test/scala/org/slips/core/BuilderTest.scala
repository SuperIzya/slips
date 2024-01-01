package org.slips.core

import cats.Eq
import cats.Eval
import cats.data.State
import org.slips.Env
import org.slips.Environment as SEnv
import org.slips.SimpleEnvironment
import org.slips.core.Empty
import org.slips.core.build.BuildContext
import org.slips.core.build.Builder
import org.slips.core.build.BuildStep
import org.slips.core.build.SelectedPredicatesAndSources
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.network.alpha.AlphaNetwork
import org.slips.core.rule.Rule
import org.slips.syntax.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object BuilderTest extends ZIOSpecDefault {

  val notApple: Fact[Fruit] => Predicate = _.test(_.name != "apple")
  private val testFruitAndVegie          = testFruitAndVegieF.tupled
  private val vegie2Fruits               = vegie2FruitsF.tupled
  private val condition1                 = for {
    h     <- all[Herb]
    b     <- all[Herb]
    berry <- all[Berry]
    _     <- berry.test(_.origin != Origin.Field)
    _     <- (b.value(_.origin) =!= Origin.GreenHouse) && b.test(_.name.nonEmpty)
    _     <- h.test(_.name.nonEmpty)
    f1    <- all[Fruit]
    _     <- f1.value(_.sugar) =!= 1.0
    f2    <- all[Fruit]
    _     <- notApple(f2) || notApple(f1)
    v     <- all[Vegetable]
    _     <- (f1, v).testMany(testFruitAndVegie)
    _     <- h.value(_.name) =!= f1.value(_.name)    
    _5 = literal(5)
    //_ <- (v, f1, f2).testMany(vegie2Fruits)
  } yield (f1, f2, v, _5)

  private def rule1(using env: SimpleEnvironment) =
    condition1.makeRule("Test rule 1") { case (f1, f2, v, c5) =>
      for {
        x1 <- f1.value
        vv <- v.value
      } yield ()
    }

  private val predicates = suite("Predicates should have same signature")({
    case class Asserts(seq: Seq[(String, TestResult)]) {
      def addStep(s: (String, TestResult)): Asserts       = copy(seq = seq :+ s)
      def addSteps(s: Seq[(String, TestResult)]): Asserts = copy(seq = seq ++ s)
    }

    def method(f: Fact[Fruit]): Predicate                    = f.test(_.name != "apple")
    def paramMethod(name: String)(f: Fact[Fruit]): Predicate = f.test(_.name != name)

    val notAppleF: Fact[Fruit] => Predicate = method(_)

    val testSeq = SimpleEnvironment { env ?=>
      type Step[T] = State[Asserts, T]
      def predicate(name: String)(cond: Condition[Fruit]): Step[Option[String]] = State { asserts =>
        val set: Set[Predicate] = Builder.selectPredicatesAndSources(cond).predicates.keySet

        asserts.addStep {
          s"condition created by $name should have only one predicate" -> assert(set)(hasSize(equalTo(1)))
        } -> set.headOption.map(_.signature).map(_.compute)

      }

      for {
        m       <- predicate("pure method") { all[Fruit].withFilter(method) }
        param   <- predicate("parametric method") { all[Fruit].withFilter(paramMethod("apple")) }
        partial <- predicate("function from partial application of pure method") { all[Fruit].withFilter(notAppleF) }
        literal <- predicate("inplace typing") { all[Fruit].withFilter(_.test(_.name != "apple")) }
        _       <- State.modify[Asserts](_.addSteps {
          Seq(
            "created by method should be the same as created by partial application" -> assertTrue(m == partial),
            "created by partial application should be the same as created literally" -> assertTrue(partial == literal),
            "created by parametric method should not be the same as created literally" -> assertTrue(param != literal)
          )
        })
        asserts <- State.get[Asserts]
      } yield asserts.seq

    }

    val res = testSeq.run(Asserts(Seq.empty)).value._2

    res.map { case (name, check) => test(name)(check) }
  }*)

  private val predicateSelectionStrategy = suite(
    "Condition parser should find all predicates and sources with respect to Environment.predicateSelectionStrategy"
  )(
    test("PredicateSelection.Keep") {
      object SEKeep extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Keep
      }

      val res = SEKeep {
        Builder.selectPredicatesAndSources(condition1)
      }

      assert(res.sources)(hasSize(equalTo(4))) &&
      assert(res.facts.filter(_.isAlpha))(hasSize(equalTo(9)))
    },
    test("PredicateSelection.Clean") {
      object SEClean extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val res: SelectedPredicatesAndSources = SEClean {
        Builder.selectPredicatesAndSources(condition1)
      }

      assert(res.sources)(hasSize(equalTo(3))) &&
      assert(res.facts.filter(_.isAlpha))(hasSize(equalTo(6)))
    }
  )
  private val alphaNetwork               = suite("Alpha network")(
    test("is built") {
      val res: AlphaNetwork = SimpleEnvironment {
        val steps: BuildStep[AlphaNetwork] = for {
          _       <- Builder.parse(rule1)
          network <- Builder.buildAlphaNetwork
        } yield network

        steps.runA(BuildContext.empty).value
      }
      assertTrue(
        res.alphaNetwork.nonEmpty,
        res.topChains.nonEmpty
      )
    }
  )

  given signatureStrategy: Signature.Strategy = Signature.Strategy.Content

  def testFruitAndVegieF(f: Fruit, v: Vegetable): Boolean = false

  def vegie2FruitsF(v: Vegetable, f1: Fruit, f2: Fruit): Boolean = true

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BuilderTest")(
    predicates,
    predicateSelectionStrategy,
    alphaNetwork
  )

  case class Fruit(name: String, sugar: Double, acidity: Double)

  case class Vegetable(name: String, origin: Origin)

  case class Herb(name: String, origin: Origin)

  case class Berry(name: String, origin: Origin)

  object Origin {
    given Empty[Origin] with {
      override def empty: Origin = Origin.GreenHouse
    }

    given Eq[Origin] with {
      override def eqv(x: Origin, y: Origin): Boolean = x == y
    }
  }

  enum Origin {
    case Field, GreenHouse
  }
}
