# Lab 11

## Case 9 | Task 1

`override def main() = branch(sense1)(rep(0){e => branch(e < 1000)(e + 1)(e)})(0)`

using `branch` function when a sensor turn on, the counter restart from 0.
otherwise if let's use `mux` function, when sensor turn on the counter will keep counting.

## Case 12 | Task 2

The goal is to spread the node's id number of a active sensor  to its neighbors.

`override def main() = foldhood(Set[ID]())((s, id) => s++id)(nbr{branch(sense1)(Set(mid()))(Set())})`
 to perform is possible use the foldhood function, similar to a fold function.
It takes an initial value (an empty Set[ID]), an accumulation function (a function that take a id set ad an id, it return a concatenation of this two)
and a mapping function (for each node neighbour of sense1, if sense1 is true it return a set with current id node, otherwise an empty set).


## Case 8 | Task 3
The goal is to put in each node, the id node nearest.
`override def main() = minHoodPlus(nbrRange, nbr{mid})._2`

using minHoodPlus function, it takes two parameters, the first is the distance between the node and its neighbour,
the second is the id of the neighbour.
The function return the second value of this tuple, it represents the id of the neighbour nearest.

## Case 14 | Task 4

The final goal is to spread the maximum id value of a ID inside a linked network.
`override def main() = rep(0) { x => maxHoodPlus(x max mid())}`
Initialized to 0, it is compared with the maximum value of the neighbours and the current node.
maxHoodPlus(x max mid()) return the maximum value between the current node and the neighbours.


## Case 16 | Task 5
The goal is to stretches the value of nodes identified by sense2 = true.
`d => mux[Double](sense1) {0.0} {mux(sense2)(minHoodPlus(nbr{d} + nbrRange*5))(minHoodPlus(nbr{d} + nbrRange)) }`

If a node is activated by sense1 true, it became 0.0.
Then each node contain the distance from the sense1.
If there are some sense2 true, the distance of this node is multiplied by 5, so each node which is near to sense2 will have a higher value.

## Partition Task 6

```
def partition(sourceId : Set[Int]) = rep(Double.MaxValue, Int.MaxValue):
  d => mux[(Double, Int)](sourceId.contains(mid)){(0.0, mid())}{minHoodPlus(nbr{d._1}+nbrRange, d._2 min nbr(d._2))}

override def main() = partition(Set(1))
```

`sourceId.contains(mid)` check if the current node is contained in the sourceId set, if true `{(0.0, mid())}` the distance is 0.0 and the id is the current node id.
If the current node is not in the sourceId set, it calculates the minimum distance between the current node with its neighbours and the minimum node id value between current id node and id nodes of its neighbours.

To make the partition function general-like, it takes a Set of IDs as parameters.

## Channel Task 7
```
class MainTask7Channel extends AggregateProgramSkeleton:

  import Builtins.Bounded.*
  
  var blockDistance: Double = 0.0
  
  def gradient(source: Boolean) = rep(Double.MaxValue):
  d => mux(source){0.0}{minHoodPlus(nbr{d} + nbrRange)}
  
  def broadcast(source: Boolean, input: Double) = rep((Double.MaxValue, Double.MaxValue)):
  d => mux(source){(0.0, input)}{minHoodPlus(nbr{d._1}+nbrRange(), nbr{d._2})}
  
  def distance(source: Boolean, destination: Boolean) =
  broadcast(source, gradient(destination))
  
  def dilate(region: Boolean, width: Double)=
  gradient(region) < width
  
  def channel(source: Boolean, destination: Boolean, width: Double) =
  dilate((gradient(source) + gradient(destination)) <= distance(source, destination)._2, width)
 
object Task7Channel extends Simulation[MainTask7Channel]
```

* The gradient function calculates the gradient from a source.

* The broadcast function takes a source and an input and then propagates the input value to the neighbors nearest to the source.

* the distance function gets a source and a destination. The distance is calculated by propagating the gradient from the destination.

* After all, the channel function takes a source, a destination and a width; it combines these functions and adds a dilatation with an arbitrary value.
