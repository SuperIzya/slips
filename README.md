![Scala Steward](https://github.com/SuperIzya/slips/actions/workflows/steward.yaml/badge.svg)
![Dependabot](https://github.com/SuperIzya/slips/actions/workflows/dependabot.yaml/badge.svg)
![Compile](https://github.com/SuperIzya/slips/actions/workflows/build.yaml/badge.svg)
![Tests](https://github.com/SuperIzya/slips/actions/workflows/test.yaml/badge.svg)

# SLIPS
## Scala language integrated production system

#### Motivation and goals
Rule-based engine, being as niche as it is, when there is a need for it - it is very useful indeed.  

Although there are rule based engines for other languages, non was found (by me) for Scala.
Sure, there are some for Java but those I know of are proprietary and pricey.
Current project is (work-in-progress) open-source rule-based engine for Scala 3.1.3 (not tested with other versions).

All the rule engines are based on algorithm [RETE](https://en.wikipedia.org/wiki/Rete_algorithm)
but none of them implements the possibility to change the rule set during run-time. 
One of the current project's goals is to close this gap.

### Production system
Production system is a facts base and a set of rules.

When a set of facts satisfies all conditions of some rule,
this rule is activated.

When rule is activated, eventually it's right-hand-side, action, will be executed.
This activation might change the fact base (or it might somehow change set of rules) triggering new activations.

The system can work in two modes:
1. until there are no new activations and then stop
2. when there is no new activations, the system becomes dormant awaiting new facts (implied a producer connect to the set of facts)

#### Fact base
A storage for all the globally-available data used by the system. 

#### Fact
Fact has several roles in the system:
1. in predicate, it represents an entity being scrutinized
2. in condition, it links together different predicates
3. in action, it provides a way to manipulate fact base

#### Environment
`org.slips.Environment` is parsing the rules and compiling them into [RETE](https://en.wikipedia.org/wiki/Rete_algorithm).

`Environment` defines following critical parts:

###### Effect
Effect type to be used in actions, e.g. `org.slips.SimpleEnvironment` defines environment for effect of type `cats.Id`. `Environment` internally uses the same effect for all effectfull actions. 
###### Parse strategy
Consider example
```scala
val condition = for {
  f <- all[Fruit]                    // 1
  v <- all[Vegetable]                // 2
  _ <- v.value(_.name) =:= "tomato"  // 3
  _ <- f.value(_.name) =!= "apple"   // 4
} yield v
```
Result of this condition is a fact of type `Vegetable`, which name is "tomato". 
Since fact `f` is not in the `yield` expression and not connected to `v` through some test.

Parse strategy answers the question should the lines 1 & 4 be in resulting network.

* `org.slips.core.build.strategy.PredicateSelection.Clean` - will remove lines 1 & 4
* `org.slips.core.build.strategy.PredicateSelection.Keep` - will keep all the lines, incorporating line 1 & 4 into resulting network and not activating rule if there is no `Fruite` that is not apple. 

#### Rule
Rule consists of set of conditions and an action that should be taken when a set of facts satisfies all the conditions.
DSL for scala provides set of monadic types to define rules.
 
```scala
import org.slips.Environment
import org.slips.syntax.*
val ingridients = for {
  z <- all[Vegetable] if v.test(_.name == "zucchini")
  a <- all[Fruit] if a.test(_.name == "apple") && a.value(_.color) =:= Color.Red
} yield (z, a)

val pancakes = (env: Environment) ?=> ingridients
  .makeRule("pancakes")
  .withAction {
  case (fZucchini, fApple) =>
    for {
      zucchini <- fZucchini.value
      apple <- fApple.value
      _ <- makePancakes(zucchini, apple)
      _ <- remove((fZucchini, fApple))
    } yield ()
}
```

###### Condition
Since condition is on a fact, each condition should start with one.
The sort of conditions to produce that initial fact is `org.slips.core.conditions.Condition.Source`.

Currently, available as set of quantors:
* `all[F]` - activates for each object of type `F` from fact base  

Result of a condition can be only of type `org.slips.core.fact.Fact[T]` or `Tuple.Map[T, Fact]` or `Unit` (as it is being empty tuple)
```scala
val fruit = for {
  f <- all[Fruit]
  _ <- f.value(_.name) =!= "apple"
} yield f

val vegetable = for {
  v <- all[Vegetable] if v.value(_.name) =:= "tomato"  
} yield v

val full = for {
  f <- all[Fruit]
  v <- all[Vegetable] if v.value(_.name) =:= "apple"
  c <- all[Cheese]
} yield (f, v, c)

val dummy = for {
  _ <- all[Fruit]
} yield ()
```

Condition is a monadic type.
```scala
val vegan = for {
  f <- fruit
  v <- vegetable
} yield (f, v)
```

Since the system is fully integrated into Scala it is possible to use external functions in conditions.
It is strongly advisable to use only pure functions. There is no guaranty of the locality where this code will be running.
It is possible to imagine distributed `Environment` that places nodes of the network on different machines.
```scala
def goodWithCheese(cheese: Vegetable): Boolean
val cheesePlate = for {
  (f, v) <- vegan
  _ <- v.test(goodWithCheese)
  _ <- f.value(_.name) =:= "grape"
  c <- all[Cheese] if c.test(_.weight >= 0.25)
} yield (f, c)
```
###### Predicate
Predicate is one of condition's possible parts, but this part is most commonly used. It represents an actual condition as simple as it may be.
One of the main advantages of [RETE](https://en.wikipedia.org/wiki/Rete_algorithm) in reuse of these predicates. So it is much advisable to reuse predicates as well
```scala
extension (f: Fact[Cheese]) {
  def isBigEnough: Predicate = f.test(_.weight >= 0.65)
}
val bigCheesePlate = for {
  (f, v) <- vegan
  _ <- v.test(goodWithCheese)
  _ <- f.value(_.name) =:= "grape"
  c <- all[Cheese] if c.isBigEnough
} yield (f, v, c)

val bigCheese = all[Cheese].withFilter(_.isBigEnough)
```

###### Action
This is the part of the rule that is being executed with a set of values that satisfies all the conditions.
In action the following results are expected:
1. changes of the factbase's state. Either by adding new facts, deleting existing ones, or changing them (which is done by replacing the old version by a new version)
1. changes to the state of the outside world by having some effect executed (writing to DB, printing on screen, etc.)

Although action is a monadic type and can be combined as thus, the result of the action in the rule is always `Unit`,
since the result of the action inside of the rule is totally meaningless otherwise.


