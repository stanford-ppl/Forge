package dhdl.compiler.ops

import org.encog.Encog
import org.encog.engine.network.activation.ActivationSigmoid
import org.encog.ml.data.{MLData, MLDataPair, MLDataSet}
import org.encog.ml.data.basic.{BasicMLData, BasicMLDataSet}
import org.encog.neural.networks.BasicNetwork
import org.encog.neural.networks.layers.BasicLayer
import org.encog.neural.networks.training.propagation.resilient.ResilientPropagation
import org.encog.persist.EncogDirectoryPersistence

import scala.collection.JavaConverters._
import scala.io.Source
import java.io.File

abstract class AbstractNeuralModel {

  val RBRAM  = 11
  val RLUTS  = 12
  val FREGS  = 13
  val UNVAIL = 14
  val IMPL   = 15
  val NEEDED = 16
  val filename = "StratixV-Routing.csv" // TODO: Shouldn't be hardcoded

  val OUT: Int
  val LAYER2: Int
  val name: String
  var network: BasicNetwork = null
  def needsInit = network eq null
  val verbose = false
  val MAX_EPOCH = 600

  val pwd = sys.env("HYPER_HOME")

  // TODO: Move these to the first couple of lines in the CSV file
  //                    LUT7      LUT6      LUT5      LUT4      LUT3      MEM64     MEM32     MEM16     Regs       DSPs    BRAM
  val maxValues = Array(262400.0, 262400.0, 524800.0, 524800.0, 524800.0, 131200.0, 262400.0, 262400.0, 1049600.0, 1963.0, 2567.0,
  /*                    RBRAM   R. LUTs   F. Regs    Unavail   Impl.     Needed  */
                        10268.0, 524800.0, 1049600.0, 262400.0, 262400.0, 262400.0)

  def init() {
    val encogFile = s"${pwd}/data/${name}.eg"
    val exists = new File(encogFile).exists

    if (exists) {
      println("Loaded " + name + " model from file")
      network = EncogDirectoryPersistence.loadObject(new File(encogFile)).asInstanceOf[BasicNetwork]
    }
    else {
      val MODELS = 1000

      val data = Source.fromFile(s"${pwd}/data/${filename}").getLines().toArray.drop(1).map(_.split(",").map(_.trim.toDouble))

      // Normalize by max and offset
      val dataNorm = Array.tabulate(data.length){i =>
                       val dat = data(i)
                       Array.tabulate(dat.length){j => dat(j) / maxValues(j) }
                     }
      val input = dataNorm.map(_.take(11))
      val output = dataNorm.map(_.slice(OUT,OUT+1).map(a => a))
      if (verbose) println(output.map(_.mkString(", ")).mkString(", "))
      val trainingSet = new BasicMLDataSet(input, output)
      var iter = 0
      var minError = Double.PositiveInfinity
      var maxError = Double.PositiveInfinity
      while (iter < MODELS) {
        val (curNetwork, curError, curMax) = trainOne(trainingSet)
        if (curMax < maxError) {
          minError = curError
          maxError = curMax
          network = curNetwork
        }
        iter += 1
      }
      println(name + "\n-----------------")
      println("Neural network results:")
      println(s"Average error: ${100*minError/trainingSet.size}%")
      println(s"Maximum observed error: ${100*maxError}")

      EncogDirectoryPersistence.saveObject(new File(encogFile), network)
    }
  }

  def trainOne(trainingSet: BasicMLDataSet) = {
    val network = new BasicNetwork()
    network.addLayer(new BasicLayer(null,true,11))
    network.addLayer(new BasicLayer(new ActivationSigmoid(),true,LAYER2))
    network.addLayer(new BasicLayer(new ActivationSigmoid(),false,1))
    network.getStructure().finalizeStructure()
    network.reset()

    var epoch = 1
    val train = new ResilientPropagation(network, trainingSet)
    train.iteration()
    while (epoch < MAX_EPOCH) {
      //println(s"Epoch #$epoch Error: ${100*train.getError()}")
      epoch += 1
      train.iteration()
    }
    train.finishTraining()
    //
    //println(s"Completed training at epoch $epoch with error of ${100*train.getError()}")

    var errors = 0.0
    var maxError = 0.0
    for (pair <- trainingSet.asScala) {
      val output = network.compute(pair.getInput())
      val data = Array.tabulate(11){i => pair.getInput().getData(i) * maxValues(i) }.mkString(", ")
      val diff = output.getData(0) - pair.getIdeal().getData(0)
      val error = diff / pair.getIdeal().getData(0)
      //println(s"output = ${output.getData(0) * maxValues(RLUTS)}, ideal = ${pair.getIdeal().getData(0) * maxValues(RLUTS)} (error = ${100*error}%, true = ${100*trueError}%)")
      if (Math.abs(error) > maxError) maxError = Math.abs(error)
      errors += Math.abs(error)
    }

    (network, errors, maxError)
  }

  def evaluate(x: FPGAResources) = {
    if (needsInit) init()
    val input = x.toArray.zip(maxValues.slice(0,RBRAM)).map{case (n,m) => n/m}
    val output = network.compute(new BasicMLData(input))
    (output.getData(0) * maxValues(OUT)).toInt
  }
}

class LUTRoutingModel extends AbstractNeuralModel { val OUT = RLUTS; val name = "RoutingLUTs"; val LAYER2 = 6 }
class RegFanoutModel extends AbstractNeuralModel { val OUT = FREGS; val name = "FanoutRegisters"; val LAYER2 = 6 }
class UnavailALMsModel extends AbstractNeuralModel { val OUT = UNVAIL; val name = "UnavailableALMs"; val LAYER2 = 6 }




class TileLoadModel {
  val name = "TileLoadModel"
  val filename = "TileLoadData.csv" // TODO: Shouldn't be hardcoded

  var network: BasicNetwork = null
  def needsInit = network eq null
  val verbose = false
  val MAX_EPOCH = 600

  val pwd = sys.env("HYPER_HOME")

  def init() {
    val encogFile = s"${pwd}/data/${name}.eg"
    val exists = new File(encogFile).exists

    if (exists) {
      println("Loaded " + name + " model from file")
      network = EncogDirectoryPersistence.loadObject(new File(encogFile)).asInstanceOf[BasicNetwork]
    }
    else {
      val MODELS = 1000

      val data = Source.fromFile(s"${pwd}/data/${filename}").getLines().toArray.drop(1).map(_.split(",").map(_.trim.toDouble))

      val input = data.map(_.take(4))

      val output = data.map(_.slice(4,5))
      if (verbose) println(output.map(_.mkString(", ")).mkString(", "))
      val trainingSet = new BasicMLDataSet(input, output)
      var iter = 0
      var minError = Double.PositiveInfinity
      var maxError = Double.PositiveInfinity
      while (iter < MODELS) {
        val (curNetwork, curError, curMax) = trainOne(trainingSet)
        if (curMax < maxError) {
          minError = curError
          maxError = curMax
          network = curNetwork
        }
        iter += 1
      }
      println(name + "\n-----------------")
      println("Neural network results:")
      println(s"Average error: ${100*minError/trainingSet.size}%")
      println(s"Maximum observed error: ${100*maxError}")

      EncogDirectoryPersistence.saveObject(new File(encogFile), network)
    }
  }

  def trainOne(trainingSet: BasicMLDataSet) = {
    val network = new BasicNetwork()
    network.addLayer(new BasicLayer(null,true,4))
    network.addLayer(new BasicLayer(new ActivationSigmoid(),true,6))
    network.addLayer(new BasicLayer(new ActivationSigmoid(),false,1))
    network.getStructure().finalizeStructure()
    network.reset()

    var epoch = 1
    val train = new ResilientPropagation(network, trainingSet)
    train.iteration()
    while (epoch < MAX_EPOCH) {
      //println(s"Epoch #$epoch Error: ${100*train.getError()}")
      epoch += 1
      train.iteration()
    }
    train.finishTraining()
    //
    //println(s"Completed training at epoch $epoch with error of ${100*train.getError()}")

    var errors = 0.0
    var maxError = 0.0
    for (pair <- trainingSet.asScala) {
      val output = network.compute(pair.getInput())
      val diff = output.getData(0) - pair.getIdeal().getData(0)
      val error = diff / pair.getIdeal().getData(0)

      if (Math.abs(error) > maxError) maxError = Math.abs(error)
      errors += Math.abs(error)
    }

    (network, errors, maxError)
  }

  def evaluate(c: Int, r: Int, b: Int, p: Int) = {
    if (needsInit) init()
    val input = Array(c.toDouble/15, r.toDouble/10000, b.toDouble/(96*256), p.toDouble/192)
    val output = network.compute(new BasicMLData(input))
    output.getData(0)
  }
}
