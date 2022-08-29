package org.slips.core

import Assertion._
import cats.Eval
import cats.data.State
import org.slips.{Environment => SEnv}
import org.slips.SimpleEnvironment
import org.slips.core.Empty
import org.slips.core.build.Builder
import org.slips.core.build.Builder.SelectedPredicatesAndSources
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import zio.Scope
import zio.test._

object BuilderTest extends ZIOSpecDefault {
  enum Origin {
    case Field, GreenHouse
  }

  object Origin {
    given Empty[Origin] with {
      override def empty: Origin = Origin.GreenHouse
    }
  }

  case class Fruit(name: String, sugar: Double, acidity: Double)
  case class Vegetable(name: String, origin: Origin)
  case class Herb(name: String, origin: Origin)
  case class Berry(name: String, origin: Origin)

  val condition1 = (env: SEnv) ?=> {
    import env.Syntax.Conditions.*
    def testFruitAndVegieF(f: Fruit, v: Vegetable): Boolean        = false
    val notApple: Fact[Fruit] => Predicate                         = _.test(_.name != "apple")
    val testFruitAndVegie                                          = (testFruitAndVegieF _).tupled
    def vegie2FruitsF(v: Vegetable, f1: Fruit, f2: Fruit): Boolean = true
    val vegie2Fruits                                               = vegie2FruitsF.tupled
    for {
      h     <- all[Herb]
      b     <- all[Herb]
      berry <- all[Berry] if berry.test(_.origin != Origin.Field)
      _     <- b.test(_.origin != Origin.GreenHouse) && b.test(_.name.nonEmpty)
      _     <- h.test(_.name.nonEmpty)
      f1    <- all[Fruit] if f1.value(_.sugar) =!= 1
      f2    <- all[Fruit] if notApple(f2) || notApple(f1)
      v     <- all[Vegetable]
      _     <- (f1, v).test(testFruitAndVegie)
      _     <- h.value(_.name) =!= f1.value(_.name)
      _5 = Fact.literal(5)
      _ <- (v, f1, f2).test(vegie2Fruits)
    } yield (f1, f2, v, _5)
  }

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BuilderTest")(predicates, strategy)

  val predicates = suite("Predicates should have same signature")({
    case class Asserts(seq: Seq[(String, TestResult)]) {
      def addStep(s: (String, TestResult)): Asserts       = copy(seq = seq :+ s)
      def addSteps(s: Seq[(String, TestResult)]): Asserts = copy(seq = seq ++ s)
    }

    def method(f: Fact[Fruit]): Predicate                    = f.test(_.name != "apple")
    def paramMethod(name: String)(f: Fact[Fruit]): Predicate = f.test(_.name != name)
    val notApleF: Fact[Fruit] => Predicate                   = method(_)

    val testSeq = SimpleEnvironment { env ?=>
      type Step[T] = State[Asserts, T]
      def predicate(name: String)(cond: Condition[Fruit]): Step[Option[String]] = State { asserts =>
        val set: Set[Predicate] = Builder.sourcesAndPredicates(cond).predicates.values.toSet.flatten

        asserts.addStep {
          s"condition created by $name should have only one predicate" -> assert(set)(hasSize(equalTo(1)))
        } -> set.headOption.map(_.signature)

      }

      for {
        m       <- predicate("pure method") {
          import env.Syntax.Conditions.*
          all[Fruit].withFilter(method)
        }
        param   <- predicate("parametric method") {
          import env.Syntax.Conditions.*
          all[Fruit].withFilter(paramMethod("apple"))
        }
        partial <- predicate("funtion from parial application of pure method") {
          import env.Syntax.Conditions.*
          all[Fruit].withFilter(notApleF)
        }
        literal <- predicate("inplace typing") {
          import env.Syntax.Conditions.*
          all[Fruit].withFilter(_.test(_.name != "apple"))
        }
        _       <- State.modify[Asserts](_.addSteps {
          Seq(
            "created by method should be the same as created by partial application" -> assert(m)(equalTo(partial)),
            "created by partial application should be the same as created literaly" -> assert(partial)(equalTo(literal)),
            "created by parametric method should not be the same as created litraly" -> assert(param)(
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

  val strategy = suite(
    "Condition parser should find all predicates and sources with respect to Environment.predicateSelectionStrategy"
  )(
    test("PredicateSelection.Keep") {
      object SEKeep extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Keep
      }

      val res = SEKeep {
        Builder.sourcesAndPredicates(condition1)
      }

      assert(res.sources)(hasSize(equalTo(4))) &&
      assert(res.predicates.values.flatten.toSet)(hasSize(equalTo(10)))
    },
    test("PredicateSelection.Clean") {
      object SEClean extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val res = SEClean {
        Builder.sourcesAndPredicates(condition1)
      }

      assert(res.sources)(hasSize(equalTo(3))) &&
      assert(res.predicates.values.flatten.toSet)(hasSize(equalTo(6)))
    }
  )
}
