package org.slips.core

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.*
import org.slips.Environment
import org.slips.core.build.Builder
import org.slips.core.build.Builder.SelectedPredicatesAndSources
import org.slips.core.build.strategy.PredicateSelection
import org.slips.core.fact.Fact
import org.slips.core.predicates.Predicate
import org.slips.SimpleEnvironment
import org.slips.core.TypeOps.Empty
import org.slips.core.conditions.Condition

class BuilderTest extends AnyFreeSpec {
  enum Origin:
    case Field, GreenHouse

  object Origin:
    given Empty[Origin] with {
      override def empty: Origin = Origin.GreenHouse
    }
    
  case class Fruit(name: String, sugar: Double, acidity: Double)
  case class Vegetable(name: String, origin: Origin)
  case class Herb(name: String, origin: Origin)
  case class Berry(name: String, origin: Origin)

  val condition1 = (env: Environment) ?=> {
    import env.Syntax.Conditions.*
    def testFruitAndVegieF(f: Fruit, v: Vegetable): Boolean = false
    val notApple: Fact[Fruit] => Predicate                  = _.test(_.name != "apple")
    val testFruitAndVegie                                   = (testFruitAndVegieF _).tupled
    for {
      h  <- all[Herb]
      b <- all[Herb]
      berry <- all[Berry] if berry.test(_.origin != Origin.Field)
      _  <- b.test(_.origin != Origin.GreenHouse) && b.test(_.name.nonEmpty)
      f1 <- all[Fruit] if f1.value(_.sugar) =!= 1
      f2 <- all[Fruit] if notApple(f2) || notApple(f1)
      v  <- all[Vegetable]
      _  <- (f1, v).test(testFruitAndVegie)
      _ <- h.value(_.name) =!= f1.value(_.name)
      _5 = Fact.literal(5)
    } yield (f1, f2, v, _5)
  }

  "Predicates should have same signature" - {
    def method(f: Fact[Fruit]): Predicate = f.test(_.name != "apple")
    def paramMethod(name: String)(f: Fact[Fruit]): Predicate = f.test(_.name != name)
    val notApleF: Fact[Fruit] => Predicate = method(_)
    SimpleEnvironment { env ?=>
      def predicate(cond: Condition[Fruit]): String = {
        import Matchers.*
        val set: Set[Predicate] = Builder.sourcesAndPredicates(cond).predicates.values.toSet.flatten
        set should have size 1
        set.head.signature
      }

      val m: String = predicate{
        import env.Syntax.Conditions.*
        all[Fruit].withFilter(method)
      }
      val param: String = predicate{
        import env.Syntax.Conditions.*
        all[Fruit].withFilter(paramMethod("apple"))
      }
      val patial: String = predicate{
        import env.Syntax.Conditions.*
        all[Fruit].withFilter(notApleF)
      }
      val literal: String = predicate{
        import env.Syntax.Conditions.*
        all[Fruit].withFilter(_.test(_.name != "apple"))
      }

      def test() = {
        import Matchers.*
        "created by method should be the same as created by partial application" in { m shouldBe patial }
        "created by partial application should be the same as created literaly" in { patial shouldBe literal }
        "created by parametric method should not be the same as created litraly" in { param should not be literal }
      }
      test()
    }
  }

  "Condition parser should find all predicates and sources with respect to Environment.predicateSelectionStrategy" - {

    "PredicateSelection.Keep" in {
      import Matchers.*
      object SEKeep extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Keep
      }

      val SelectedPredicatesAndSources(predicates, sources, _) = SEKeep {
        Builder.sourcesAndPredicates(condition1)
      }

      sources should have size 4
      predicates.values.flatten.toSet should have size 8
    }
    "PredicateSelection.Clean" in {
      import Matchers.*
      object SEClean extends SimpleEnvironment {
        override val predicateSelectionStrategy: PredicateSelection = PredicateSelection.Clean
      }
      val SelectedPredicatesAndSources(predicates, sources, _) = SEClean {
        Builder.sourcesAndPredicates(condition1)
      }

      sources should have size 3
      predicates.values.flatten.toSet should have size 5
    }

  }
}
