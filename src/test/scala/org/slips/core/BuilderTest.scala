package org.slips.core

import cats.Eval
import cats.data.State
import org.slips.Env
import org.slips.Environment as SEnv
import org.slips.Signature
import org.slips.SimpleEnvironment
import org.slips.core.BuilderTest.vegie2Fruits
import org.slips.core.Empty
import org.slips.core.build.BuildContext
import org.slips.core.build.Builder
import org.slips.core.build.BuildStep
import org.slips.core.build.SelectedPredicatesAndSources
import org.slips.core.build.strategy.AlphaNodeStrategy
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.network.AlphaNetwork
import org.slips.core.predicates.Predicate
import org.slips.core.rule.Rule
import org.slips.syntax.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object BuilderTest extends ZIOSpecDefault {
  val notApple: Fact[Fruit] => Predicate = _.test(_.name != "apple")
  private val testFruitAndVegie          = (testFruitAndVegieF _).tupled
  private val vegie2Fruits               = vegie2FruitsF.tupled
  private val condition1                 = for {
    h     <- all[Herb]
    b     <- all[Herb]
    berry <- all[Berry] if berry.test(_.origin != Origin.Field)
      && b.test(_.origin != Origin.GreenHouse) && b.test(_.name.nonEmpty)
      && h.test(_.name.nonEmpty)
    f1    <- all[Fruit] if f1.value(_.sugar) =!= 1
    f2    <- all[Fruit] if notApple(f2) || notApple(f1)
    v     <- all[Vegetable] if (f1, v).test(testFruitAndVegie) &&
      h.value(_.name) =!= f1.value(_.name) &&
      (v, f1, f2).test(vegie2Fruits)
    _5 = Fact.literal(5)
  } yield (f1, f2, v, _5)

  private val rule1: SimpleEnvironment ?=> Rule.RuleM = (env: SimpleEnvironment) ?=>
    condition1
      .makeRule("Test rule 1")
      .withAction { case (f1, f2, v, c5) =>
        for {
          x1 <- f1.value
        } yield ()
      }
  private val predicates                              = suite("Predicates should have same signature")({
    case class Asserts(seq: Seq[(String, TestResult)]) {
      def addStep(head: (String, TestResult), tail: (String, TestResult)*): Asserts =
        copy(seq = (seq :+ head) ++ tail.toSeq)

      def addSteps(s: Seq[(String, TestResult)]): Asserts = copy(seq = seq ++ s)
    }

    def method(f: Fact[Fruit]): Predicate                    = f.test(_.name != "apple")
    def paramMethod(name: String)(f: Fact[Fruit]): Predicate = f.test(_.name != name)
    val notApleF: Fact[Fruit] => Predicate                   = method(_)

    val testSeq = SimpleEnvironment { env ?=>
      type Step[T] = State[Asserts, T]
      def predicate(name: String)(cond: Condition[Fruit]): Step[Option[Signature]] = State { asserts =>
        val set: Set[Predicate] = Builder.selectPredicatesAndSources(cond).predicates.values.toSet.flatten

        asserts.addStep {
          s"condition created by $name should have only one predicate" -> assert(set)(hasSize(equalTo(1)))
        } -> set.headOption.map(_.signature)

      }

      for {
        m       <- predicate("pure method") {
          all[Fruit].withFilter(method)
        }
        param   <- predicate("parametric method") {
          all[Fruit].withFilter(paramMethod("apple"))
        }
        partial <- predicate("funtion from parial application of pure method") {
          all[Fruit].withFilter(notApleF)
        }
        literal <- predicate("inplace typing") {
          all[Fruit].withFilter(_.test(_.name != "apple"))
        }
        _       <- State.modify[Asserts](_.addSteps {
          Seq(
            "created by method should be the same as created by partial application" -> assert(m)(equalTo(partial)),
            "created by partial application should be the same as created literally" -> assert(partial)(equalTo(literal)),
            "created by parametric method should not be the same as created literally" -> assert(param)(
              not(equalTo(literal))
            )
          )
        })
        asserts <- State.get[Asserts]
      } yield asserts.seq

    }

    val res = testSeq.run(Asserts(Seq.empty)).value._2

    res.map { case (name, check) =>
      test(name)(check)
    }
  }: _*)
  private val predicateSelectionStrategy              = suite(
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
      assert(res.predicates.values.flatten.toSet)(hasSize(equalTo(9)))
    },
    test("PredicateSelection.Clean") {
      object SEClean extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val res: SelectedPredicatesAndSources = SEClean {
        Builder.selectPredicatesAndSources(condition1)
      }

      assert(res.sources)(hasSize(equalTo(3))) &&
      assert(res.predicates.values.flatten.toSet)(hasSize(equalTo(6)))
    }
  )
  private val alphaNodeStrategy = suite("Alpha network should be build with respect to Environment.alphaNodeStrategy")(
    test("AlphaNodeStrategy.MaximumUtil - needs to be optimized") {
      object SEMaxUtil extends SimpleEnvironment {
        override val alphaNodeStrategy: AlphaNodeStrategy           = AlphaNodeStrategy.MaximizeChains
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val res: AlphaNetwork = SEMaxUtil {
        val steps: BuildStep[AlphaNetwork] = for {
          _       <- Builder.parse(rule1)
          network <- Builder.buildAlphaNetwork
        } yield network

        steps.runA(BuildContext.empty).value
      }
      assertTrue(
        res.sources.nonEmpty,
        res.topNodes.nonEmpty
      )
    },
    test("AlphaNodeStrategy.MinimumBuffers") {
      object SEMinBuffs extends SimpleEnvironment {
        override val alphaNodeStrategy: AlphaNodeStrategy           = AlphaNodeStrategy.MinimumBuffers
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val res: AlphaNetwork = SEMinBuffs {
        val steps: BuildStep[AlphaNetwork] = for {
          _       <- Builder.parse(rule1)
          network <- Builder.buildAlphaNetwork
        } yield network

        steps.runA(BuildContext.empty).value
      }
      assertTrue(
        res.sources.nonEmpty,
        res.topNodes.nonEmpty
      )
    }
  )
  private val strategy          = suite("Strategies should work")(
    predicateSelectionStrategy,
    alphaNodeStrategy
  )

  def testFruitAndVegieF(f: Fruit, v: Vegetable): Boolean = false

  def vegie2FruitsF(v: Vegetable, f1: Fruit, f2: Fruit): Boolean = true

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BuilderTest")(
    predicates,
    strategy
  ) @@ TestAspect.timed

  case class Fruit(name: String, sugar: Double, acidity: Double)

  case class Vegetable(name: String, origin: Origin)

  case class Herb(name: String, origin: Origin)

  case class Berry(name: String, origin: Origin)

  object Origin {
    given Empty[Origin] with {
      override def empty: Origin = Origin.GreenHouse
    }
  }

  enum Origin {
    case Field, GreenHouse
  }
}
