# Fact's and predicate's signature

## Motivation

When building a network that will run the condition for the rules,
it is important to reuse predicates whenever possible.

## Example
Given following data type:
```scala
case class Box(size: (Double, Double, Double), contentWeights: Seq[Double])
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

val cond4 = for {
  box <- all[Box]
  volume = box.value(b => b.size._1 * b.size._2 * b.size._3)
  _ <- volume.test(_ > 10)
} yield box
```

### Problem
In all these examples fact `doubleWeight` is conceptually the same - double the weight of box's content.
In `cond1` and `cond2` predicate on `doubleWeight` is the same. They should share the same node in the network.

On the other hand, `cond4` predicate on `volume` does exactly the same as on `doubleWeight` in `cond2`.
But they definitely should be different nodes in the network.

To know these things the code of `value` and `test` should be thoroughly "understood". Which is some cases might be
possible only in run-time. 

### Solution

##### Glossary

*Signature* - string identifying a predicate or a fact transformation (through a call to fact's `value`) 

*Fact history* - all the transformations applied to original fact (created by `all[Box]`) until a current one. Defined by
orderly *signatures* of sequential transformations.


#### Signature

The signature can be obtained either by saving the expression inside `value` and `test` calls.
Or using hash code of the function passed as an argument. Both approaches have a huge potential for false negatives 
(the same predicates on same facts end up being different nodes in the network).

##### Expression
It is possible to take the code of expression `doubleWeight.test(_ > threshold)` and convert it to a string "_ > threshold"
(with the details of conserving the *fact history* of the fact). But then `cond1` and `cond3` share the same signature.
But will work good in cases:
```scala
val doubleSum: Box => Double = _.contentWeights.sum * 2
val cond1 = for {
    box <- all[Box]
    doubleWeight = box.value(doubleSum)
  _ <- doubleWeight.test(_ > 10)
} yield doubleWeight

val cond2 = for {
  box <- all[Box]
  doubleWeight = box.value(doubleSum)
  _ <- doubleWeight.test(_ > 10)
} yield doubleWeight

val cond3 = for {
    box <- all[Box]
    doubleWeight = box.value(doubleSum)
    _ <- doubleWeight.test(_ > 8)
} yield doubleWeight

val cond4 = for {
  box <- all[Box]
  volume = box.value(b => b.size._1 * b.size._2 * b.size._3)
  _ <- volume.test(_ > 10)
} yield box
```

##### Hash code
All inline lambdas will be separate nodes. Will work good in more complicated case:
```scala
object Functions {
  val doubleSum: Box => Double = _.contentWeights.sum * 2
  val volume: Box => Double = b => b.size._1 * b.size._2 * b.size._3
  val predicates: Map[Int, Double => Boolean] = Map(10 -> _ > 10, 8 -> _ > 8)
}
val cond1 = {
  val threshold = 10

  for {
    box <- all[Box]
    doubleWeight = box.value(Functions.doubleSum)
    _ <- doubleWeight.test(Functions.predicates(threshold))
  } yield doubleWeight
}

val cond2 = for {
  box <- all[Box]
  doubleWeight = box.value(Functions.doubleSum)
  _ <- doubleWeight.test(Functions.predicates(10))
} yield doubleWeight

val cond3 = {
  import Functions.*
  val threshold = 8

  for {
    box <- all[Box]
    doubleWeight = box.value(doubleSum)
    _ <- doubleWeight.test(predicates(threshold))
  } yield doubleWeight
}

val cond4 = for {
  box <- all[Box]
  volume = box.value(Functions.volume)
  _ <- volume.test(Functions.predicates(10))
} yield box
```

#### Provided

Of course, it is possible to provide an expression that will be evaluated at runtime into a signature.

```scala
val cond1 = {
  val threshold = 10

  for {
    box <- all[Box]
    doubleWeight = box.value(_.contentWeights.map(_ * 2).sum).signed("Double the weight")
    _ <- doubleWeight.test(_ > threshold).signed(s"_ > $threshold")
  } yield doubleWeight
}

val cond2 = for {
  box <- all[Box]
  doubleWeight = box.value(_.contentWeights.sum * 2).signed("Double the weight")
  _ <- doubleWeight.test(_ > 10).signed("_ > 10")
} yield doubleWeight

val cond3 = {
  val threshold = 8

  for {
    box <- all[Box]
    doubleWeight = box.value(_.contentWeights.map(_ * 2).sum).signed("Double the weight")
    _ <- doubleWeight.test(_ > threshold).signed(s"_ > $threshold")
  } yield doubleWeight
}

val cond4 = for {
  box <- all[Box]
  volume = box.value(b => b.size._1 * b.size._2 * b.size._3).signed("Full volume")
  _ <- volume.test(_ > 10).signed("_ > 10")
} yield box
```

With these tricks the system will be able to identify same facts transformations and same predicates.
Which allows to build more efficient RETE networks, making your rules run faster and consume less RAM. 
