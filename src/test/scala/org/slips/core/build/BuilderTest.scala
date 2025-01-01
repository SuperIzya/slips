package org.slips.core.build

import cats.Eval
import cats.data.State
import org.slips.{EnvRule, Signature, SimpleEnvironment}
import org.slips.core.*
import org.slips.core.build.*
import org.slips.core.build.ResultAssertion.*
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.*
import org.slips.core.fact.Fact
import org.slips.core.network.NetworkLayer
import org.slips.core.rule.Rule
import org.slips.data.*
import org.slips.syntax.*
import zio.*
import zio.test.Assertion.*
import zio.test.{Result as _, *}

object BuilderTest extends ZIOSpecDefault {

  val notApple: Fact[Fruit] => Predicate                            = _.test(_.name != "apple")
  private val testFruitAndVegie: ((Fruit, Vegetable)) => Boolean    = testFruitAndVegieF.tupled
  private val vegie2Fruits                                          = vegie2FruitsF.tupled
  private val condition1: Condition[(Fruit, Fruit, Vegetable, Int)] = for {
    h     <- all[Herb]
    b     <- all[Herb]
    berry <- all[Berry]
    _     <- berry.value(_.origin) =!= literal(Origin.Field)
    _     <- b.value(_.origin) =!= Origin.GreenHouse && b.test(_.name.nonEmpty)
    _     <- h.test(_.name.nonEmpty)
    f1    <- all[Fruit] if f1.test(_.sugar != 1.0)
    f2    <- all[Fruit]
    _     <- notApple(f2) || notApple(f1)
    v     <- all[Vegetable]
    _     <- (f1, v).testMany(testFruitAndVegie)
    hname = h.value(_.name)
    fname = f1.value(_.name)
    _ <- hname =!= fname
    _5 = literal(5)
    _ <- (v, f1, f2).testMany(vegie2Fruits)
  } yield (f1, f2, v, _5)

  private val rule1: EnvRule = env ?=> {
    condition1.makeRule("Test rule 1") { case (f1, f2, v, c5) =>
      for {
        x1 <- f1.value
        vv <- v.value
      } yield ()
    }
  }
  private val predicates     = suite("Predicates should have same signature")({
    case class Asserts(seq: Seq[(String, TestResult)]) {
      def addStep(s: (String, TestResult)): Asserts       = copy(seq = seq :+ s)
      def addSteps(s: Seq[(String, TestResult)]): Asserts = copy(seq = seq ++ s)
    }

    def method(f: Fact[Fruit]): Predicate                    = f.test(_.name != "apple")
    def paramMethod(name: String)(f: Fact[Fruit]): Predicate = f.test(_.name != name)

    val notAppleF: Fact[Fruit] => Predicate = method

    val testSeq = SimpleEnvironment {
      type Step[T] = State[Asserts, T]
      def predicate(name: String)(cond: Condition[Fruit]): Step[Option[String]] = State { asserts =>
        val predicatesAndSources: Result[SelectedPredicatesAndSources] = Builder.selectPredicatesAndSources(cond)

        val set: Set[Predicate] = predicatesAndSources.toOption.get.predicates

        asserts.addStep {
          s"condition created by $name should have only one predicate" -> assert(set)(hasSize(equalTo(1)))
        } -> set.headOption.map(_.signature).map(_.compute)

      }

      transparent inline def ass(a: WithSignature, b: WithSignature, inline f: (String, String) => Boolean) =
        assertTrue(f(a.signature.compute, b.signature.compute))

      for {
        m       <- predicate("pure method") { all[Fruit].withFilter(method) }
        param   <- predicate("parametric method") { all[Fruit].withFilter(paramMethod("apple")) }
        partial <- predicate("function from partial application of pure method") { all[Fruit].withFilter(notAppleF) }
        literal <- predicate("inplace typing") { all[Fruit].withFilter(_.test(_.name != "apple")) }
        _       <- State.modify[Asserts](_.addSteps {
          Seq(
            "created by method should be the same as created by partial application"   -> ass(m, partial, _ == _),
            "created by partial application should be the same as created literally"   -> ass(partial, literal, _ == _),
            "created by parametric method should not be the same as created literally" -> ass(param, literal, _ != _)
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

      val res: Result[SelectedPredicatesAndSources] = SEKeep {
        Builder.selectPredicatesAndSources(condition1)
      }

      assert(res) {
        res.assertField("sourceSignatures", _.sourceSignatures, hasSize(equalTo(4))) &&
        res.assertField("facts", _.facts, hasSize(equalTo(9)))
      }
    },
    test("PredicateSelection.Clean") {
      object SEClean extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val res: Result[SelectedPredicatesAndSources] = SEClean {
        Builder.selectPredicatesAndSources(condition1)
      }

      assert(res) {
        res.assertField("sourceSignatures", _.sourceSignatures, hasSize(equalTo(3))) &&
        res.assertField("facts", _.facts, hasSize(equalTo(5)))
      }
    }
  )

  private val network = suite("Network")(
    test("is built") {
      val res = SimpleEnvironment { env ?=>
        val parsed: Result[BuildStep[env.Effect][Unit]] = Builder.parse(rule1)
        parsed.map { parseStep =>
          val steps = for {
            _       <- parseStep
            network <- Builder.buildNetwork
          } yield network

          steps.runA(BuildContext.empty).value
        }
      }

      assert(res) {
        res.assertField("networkLayer", _.networkLayer, isNonEmpty) &&
        res.assertField("topChains", _.topChains, isNonEmpty)
      }
    }
  )

  given signatureStrategy: Signature.Strategy = Signature.Strategy.Content

  def testFruitAndVegieF(f: Fruit, v: Vegetable): Boolean = false

  def vegie2FruitsF(v: Vegetable, f1: Fruit, f2: Fruit): Boolean = true

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BuilderTest")(
    predicates,
    predicateSelectionStrategy,
    network
  )

}
