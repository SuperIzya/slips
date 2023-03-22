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
    doubleWeight = box.value(_.contentWeights.map(_ * 2).sum)
    _ <- doubleWeight.test(_ > threshold).signed(s"_ > $threshold")
  } yield doubleWeight
}
```

This way the system will be able to identify same facts transformations and same predicates.
Which allows to build more efficient RETE networks. 
