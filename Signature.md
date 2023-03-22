# Fact's and predicate's signature

## Motivation

When building a network that will run the condition for the rules,
it is important to reuse predicates whenever possible.
Given following data type:
```scala
case class Box(size: (Int, Int), contentWeights: Seq[Double])
```

Consider two following conditions:

<script src="https://gist.github.com/SuperIzya/4a75d1911c96a5bce623a3271febf3e1.js"></script>

and

<script src="https://gist.github.com/SuperIzya/b1455e0f83806dc41b14825fa92786ac.js"></script>
