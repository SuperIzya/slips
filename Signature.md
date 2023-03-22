# Fact's and predicate's signature

## Motivation

When building a network that will run the condition for the rules,
it is important to reuse predicates whenever possible.

## Example
Given following data type:
```scala
case class Box(size: (Int, Int), contentWeights: Seq[Double])
```

Consider the following conditions:

```scala
val cond1 = {
  val threshold = 10

  for {
    box <- all[Box]
    doubleWeight = box.value(_.contentWeights.map(_ * 2).sum)
    _ <- doubleWeight.test(_ > threshold)
  } yield doubleWeight
}

val cond2 = for {
  box <- all[Box]
  doubleWeight = box.value(_.contentWeights.sum * 2)
  _ <- doubleWeight.test(_ > 10)
} yield doubleWeight

val cond3 = {
  val threshold = 8
 
  for {
    box <- all[Box]
    doubleWeight = box.value(_.contentWeights.map(_ * 2).sum)
    _ <- doubleWeight.test(_ > threshold)
  } yield doubleWeight
}
```

### Problem
In all these examples fact `doubleWeight` is conceptually the same - double the weight of box's content. 
If we look at the function in the `box.value(...)` it is not always the same.

Additionally, `doubleWeight.test(...)` is actually the same in the first and second cases, but the function signature is the same in the first and third cases.

### Solution
The function can be signed:
* by a `hashCode` of the function - default option.
* by expression evaluated at runtime.

```scala
val doubleSum: Box => Double = _.contentWeights.sum * 2
val cond1 = {
  val threshold = 10

  for {
    box <- all[Box]
    doubleWeight = box.value(doubleSum)
    _ <- doubleWeight.test(_ > threshold).signed(s"_ > $threshold")
  } yield doubleWeight
}

val cond2 = for {
  box <- all[Box]
  doubleWeight = box.value(doubleSum)
  _ <- doubleWeight.test(_ > 10).signed("_ > 10")
} yield doubleWeight

val cond3 = {
  val threshold = 8
 
  for {
    box <- all[Box]
    doubleWeight = box.value(doubleSum)
    _ <- doubleWeight.test(_ > threshold).signed(s"_ > $threshold")
  } yield doubleWeight
}
```
In the first example, all `.value(...)` and `.test(...)` will be signed by hash codes of the passed functions.
Each time the instance and the hash code will be new and each of these predicates will be treated as unique,
and facts won't be able to unite the predicates - these will be different facts, passing different transformations.

In the second example:
* all instances named `doubleWeight` will be identified as same fact. 
This is because all the `box` transformations, happened with constant `doubleSum`.
* In `.test(...)`, since the function can not be common for all, each appearance is signed.

With these tricks the system will be able to identify same facts transformations and same predicates.
Which allows to build more efficient RETE networks, making your rules run faster. 
