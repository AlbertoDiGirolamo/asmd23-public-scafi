package lab.demo

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ExportEvaluation.EXPORT_EVALUATION
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.{ScafiSimulationInitializer, SimulationInfo}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.{ScafiProgramBuilder, ScafiWorldInformation}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ScafiWorldIncarnation.EXPORT
import it.unibo.scafi.simulation.s2.frontend.view.{ViewSetting, WindowConfiguration}
import it.unibo.scafi.space.graphics2D.BasicShape2D.Circle
import jdk.internal.net.http.common.Pair.pair

import scala.reflect.*

object Incarnation extends BasicAbstractIncarnation with BuildingBlocks
import lab.demo.Incarnation._ //import all stuff from an incarnation

trait Simulation[R: ClassTag] extends App:
  val formatter_evaluation: EXPORT_EVALUATION[Any] = (e : EXPORT) => formatter(e.root[Any]())

  val formatter: Any => Any = _ match
    case (a,b) => (formatter(a),formatter(b))
    case (a,b,c) => (formatter(a),formatter(b),formatter(c))
    case (a,b,c,d) => (formatter(a),formatter(b),formatter(c),formatter(d))
    case l:Iterable[_] => l.map(formatter(_)).toString
    case i: java.lang.Number if (i.doubleValue()>100000) => "Inf"
    case i: java.lang.Number if (-i.doubleValue()>100000) => "-Inf"
    case i: java.lang.Double => f"${i.doubleValue()}%1.2f"
    case x => x.toString

  val nodes = 100
  val neighbourRange = 200
  val (width, height) = (1920, 1080)

  ViewSetting.windowConfiguration = WindowConfiguration(width, height)
  ViewSetting.labelFontSize = 20
  ScafiProgramBuilder(
    Random(nodes, width, height),
    SimulationInfo(implicitly[ClassTag[R]].runtimeClass, exportEvaluations = List(formatter_evaluation)),
    ScafiSimulationInitializer.RadiusSimulation(neighbourRange),
    ScafiWorldInformation(shape = Some(Circle(5, 5))),
    neighbourRender = true,
  ).launch()

trait AggregateProgramSkeleton extends AggregateProgram with StandardSensors:
  def sense1 = sense[Boolean]("sens1")
  def sense2 = sense[Boolean]("sens2")
  def sense3 = sense[Boolean]("sens3")
  def boolToInt(b: Boolean) = mux(b){1}{0}


class Main1 extends AggregateProgramSkeleton:
  override def main() = 1
object Demo1 extends Simulation[Main1]

class Main2 extends AggregateProgramSkeleton:
  override def main() = 2+3
object Demo2 extends Simulation[Main2]

class Main3 extends AggregateProgramSkeleton:
  override def main() = (10,20)
object Demo3 extends Simulation[Main3]

class Main4 extends AggregateProgramSkeleton:
  override def main() = Math.random()

object Demo4 extends Simulation[Main4]

class Main5 extends AggregateProgramSkeleton:
  override def main() = sense1

object Demo5 extends Simulation[Main5]

class Main6 extends AggregateProgramSkeleton:
  override def main() = if (sense1) 10 else 20

object Demo6 extends Simulation[Main6]

class Main7 extends AggregateProgramSkeleton:
  override def main() = mid()

object Demo7 extends Simulation[Main7]

class Main8 extends AggregateProgramSkeleton:
  override def main() = minHoodPlus(nbrRange)

object Demo8 extends Simulation[Main8]


class MainTask3 extends AggregateProgramSkeleton:
  override def main() = minHoodPlus(nbrRange, nbr{mid})._2

object Task3 extends Simulation[MainTask4]

class Main9 extends AggregateProgramSkeleton:
  override def main() = rep(0){_+1}

object Demo9 extends Simulation[Main9]


class MainTask1 extends AggregateProgramSkeleton:
  override def main() = branch(sense1)(rep(0){e => branch(e < 1000)(e + 1)(e)})(0)
//con mux quando attivi un nodo non roncomincia da 0 ma continua a contare
//con branch quando attivi un nodo rincomincia a contare da 0
object Task1 extends Simulation[MainTask1]

class Main10 extends AggregateProgramSkeleton:
  override def main() = rep(Math.random()){x=>x}

object Demo10 extends Simulation[Main10]

class Main11 extends AggregateProgramSkeleton:
  override def main() = rep[Double](0.0){x => x + rep(Math.random()){y=>y}}
//alla prima iterazione viene genereato un numero random per ogni nodo.
//alle successive iterazioni venegono sommate ad ogni nodo il numero random generato in partenza
// non ne viene sommato un nuovo random
object Demo11 extends Simulation[Main11]

class Main12 extends AggregateProgramSkeleton:
  import Builtins.Bounded.of_i

  override def main() = maxHoodPlus(boolToInt(nbr{sense1}))
  
object Demo12 extends Simulation[Main12]


class MainTask2 extends AggregateProgramSkeleton:

  import Builtins.Bounded.of_i

  override def main() = foldhood(Set[ID]())((s, id) => s++id)(nbr{branch(sense1)(Set(mid()))(Set())})

object Task2 extends Simulation[MainTask2]


class Main13 extends AggregateProgramSkeleton:
  override def main() = foldhoodPlus(0)(_+_){nbr{1}}
  //ogni nodo ha il numero dei suoi vicini. inserisce un uno in tutti i vicini un nodo, li accumula (_+_) sommandolo e li inserisce nel nodo

object Demo13 extends Simulation[Main13]

class Main14 extends AggregateProgramSkeleton:
  import Builtins.Bounded.of_i

  override def main() = rep(0){ x => boolToInt(sense1) max maxHoodPlus( nbr{x}) }
  //alla prima iterazione tutti i nodi sono a 0.
  // se un nodo ha il sensore attivo allora il valore del nodo diventa 1 e lo propaga a tutti i nodi della rete interconnessi

object Demo14 extends Simulation[Main14]



class MainTask4 extends AggregateProgramSkeleton:

  import Builtins.Bounded.of_i

  override def main() = rep(0){ x => maxHoodPlus(nbr{x max mid()}) }

object Task4 extends Simulation[MainTask4]



class Main15 extends AggregateProgramSkeleton:
  override def main() = rep(Double.MaxValue):
    d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+1.0)}
    //partendo da un nodo (viene attivato) viene messo 0.
    //successivamente prende il minimo tra il valore dei suoi vicini (cosi in modo da escludere gli inf) sommando uno.


object Demo15 extends Simulation[Main15]

class Main16 extends AggregateProgramSkeleton:
  override def main() = rep(Double.MaxValue):
    d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+nbrRange)}
//sommano due mappe: 1) chiave = nodo | valore = valore valore sull'arco.
//                   2) chiave = nodo | valore = valore del nodo.
object Demo16 extends Simulation[Main16]


class MainTask5 extends AggregateProgramSkeleton:
  override def main() = rep(Double.MaxValue):
    d => mux[Double](sense1) {0.0} {mux(sense2)(minHoodPlus(nbr{d} + nbrRange*5))(minHoodPlus(nbr{d} + nbrRange)) }

object Task5 extends Simulation[MainTask5]

class MainTask6Partition extends AggregateProgramSkeleton:
  import Builtins.Bounded.*
  def partition(sourceId : Set[Int]) = rep(Double.MaxValue, Int.MaxValue):
    d => mux[(Double, Int)](sourceId.contains(mid)){(0.0, mid())}{minHoodPlus(nbr{d._1}+nbrRange, d._2 min nbr(d._2))}

  override def main() = partition(Set(1,2))

object Task6Partition extends Simulation[MainTask6Partition]


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
  override def main() = channel(sense1, sense2, 100.0)


object Task7Channel extends Simulation[MainTask7Channel]


class Main17 extends AggregateProgramSkeleton with BlockG:
  override def main() = gradientCast(source = sense1)(center = false)(accumulation = sense2 | _)

object Demo17 extends Simulation[Main17]

class Main18 extends AggregateProgramSkeleton with BlockG with BlockC:
  override def main() =
    val potential = gradientCast(sense1)(0.0)(_ + nbrRange)
    collectCast[Int](potential)(local = boolToInt(sense2))(Null = 0)(accumulation = _ + _)
object Demo18 extends Simulation[Main18]

class Main19 extends AggregateProgramSkeleton with BlockT:
  override def main() =
    decay(10000, 0)(_ - 1)
object Demo19 extends Simulation[Main19]
