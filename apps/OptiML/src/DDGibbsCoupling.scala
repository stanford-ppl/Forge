import optiml.compiler._
import optiml.library._
import optiml.shared._

object DDGibbsCouplingCompiler extends OptiMLApplicationCompiler with DDGibbsCoupling
object DDGibbsCouplingInterpreter extends OptiMLApplicationInterpreter with DDGibbsCoupling

trait DDGibbsCoupling extends OptiMLApplication {

  def print_usage = {
    println("Usage: DDGibbsCoupling <factors file> <variables file> <weights file> <edges file> <max samples> <number of runs>")
    exit(-1)
  }

  def evalFFX(ffx: Rep[Int], nargs: Rep[Int], args: Rep[Int] => Rep[Boolean]): Rep[Double] = {
    if(ffx == 0) { // IMPLY
      var acc = args(nargs - 1)
      var idx = 0
      while ((idx < nargs - 1)&&(acc == false)) {
        if (args(idx) == false) {
          acc = true
        }
        idx += 1
      }
      if (acc) {
        1.0
      }
      else {
        0.0
      }
    }
    else if (ffx == 1) { // OR
      var acc = false
      var idx = 0
      while ((idx < nargs)&&(acc == false)) {
        acc = args(idx)
        idx += 1
      }
      if (acc) {
        1.0
      }
      else {
        0.0
      }
    }
    else if (ffx == 2) { // AND
      var acc = true
      var idx = 0
      while ((idx < nargs)&&(acc == true)) {
        acc = args(idx)
        idx += 1
      }
      if (acc) {
        1.0
      }
      else {
        0.0
      }
    }
    else if (ffx == 3) { // EQUAL
      if(nargs == 2) {
        if (args(0) == args(1)) {
          1.0
        }
        else {
          0.0
        }
      } 
      else {
        println("error: isequal factor function cannot contain " + nargs + " arguments")
        exit(-1)
        0.0
      }
    }
    else if (ffx == 4) { // ISTRUE
      if(nargs == 1) {
        if (args(0)) {
          1.0
        }
        else {
          0.0
        }
      }
      else {
        println("error: istrue factor function cannot contain " + nargs + " arguments")
        exit(-1)
        0.0
      }
    }
    else if (ffx == 6) { //RATIO
      val bhead = args(nargs - 1)
      var acc = 1.0
      var idx = 0
      while (idx < nargs - 1) {
        if (bhead || (args(idx) == false)) {
          acc += 1.0
        }
        idx += 1
      }
      if (nargs == 1) {
        if (bhead) {
          1.0
        }
        else {
          0.0
        }
      }
      else {
        log(acc) / log(2.0)
      }
    }
    else {
      println("error: invalid factor function " + ffx)
      exit(-1)
      0.0
    }
  }

  def readFVUnder(G: Rep[DDFactorGraph], f: Rep[Int], v: Rep[Int], x: Rep[Boolean], idx: Rep[Int]): Rep[Boolean] = {
    val ie = G.f2v.ngbrEdges(f).apply(idx)
    val iv = G.f2v.edges.apply(ie)

    val vv = if(v == iv) {
      x
    }
    else {
      G.variableValue.apply(iv)
    }

    if (G.edgeIsPositiveF2V.apply(ie)) {
      vv
    }
    else {
      !vv
    }
  }

  def evalFactorUnder(G: Rep[DDFactorGraph], f: Rep[Int], v: Rep[Int], x: Rep[Boolean]): Rep[Double] = {
    val nvars = G.f2v.ngbrEdges(f).length
    val ffx = G.factorFunction.apply(f)

    val z = evalFFX(ffx, nvars, { idx => readFVUnder(G, f, v, x, idx) })
    // get the weight
    val w = G.weightValue.apply(G.factorWeightIdx.apply(f))
    // finally, return the weight times the result
    z * w
  }

  def sampleVariable(G: Rep[DDFactorGraph], v: Rep[Int], rd: Rep[Double]) {
    val dw: Rep[Double] = G.v2f.ngbrNodes(v).map({ f =>
      val w0 = evalFactorUnder(G, f, v, false)
      val w1 = evalFactorUnder(G, f, v, true)
      w1 - w0
    }).sum

    val newValue: Rep[Boolean] = ((rd * (1.0 + exp(-dw))) <= 1.0)
    G.variableValue(v) = newValue
  }

  // randomly reassign the non-evidence variables to 0 or 1
  def randomizeVariables(G: Rep[DDFactorGraph]) = {
    val GR = G.mutableVariables()

    for (iv <- 0::G.nonEvidenceVariables.length) {
      val v = G.nonEvidenceVariables.apply(iv)
      val newValue: Rep[Boolean] = (random[Double] >= 0.5)
      GR.variableValue(v) = newValue
    }

    GR
  }

  def main() = {
    if (args.length < 6) print_usage

    tic("io")
    val maxSamples = args(4).toInt
    val nruns = args(5).toInt
    val G = readDDFactorGraph(args(0), args(1), args(2), args(3))
    toc("io", G)

    println("finished reading factor graph")
    println("read:")
    println("  " + G.numFactors + " factors")
    println("  " + G.numVariables + " variables")
    println("  " + G.numWeights + " weights")
    println("  " + G.numEdges + " edges")
    println("  " + G.nonEvidenceVariables.length + " non-evidence variables")
    println("max samples: " + maxSamples)
    println("number of runs: " + nruns)

    println("")
    println("done!")

    val coupling_times = Vector[Int](nruns)

    var irun = 1
    while(irun <= nruns) {
      println("")
      println("run " + irun)
      println("creating two mutable copies of the graph...")
      val G1 = randomizeVariables(G)
      val G2 = randomizeVariables(G)
      println("done!")

      println("")
      println("estimating coupling time")

      tic("run" + irun)

      var sampleCt = 0
      var sampleMax = maxSamples
      while (sampleCt < sampleMax) {
        for (iv <- 0::G.nonEvidenceVariables.length) {
          val v = G.nonEvidenceVariables.apply(iv)
          val rd: Rep[Double] = random[Double]
          sampleVariable(G1, v, rd)
          sampleVariable(G2, v, rd)
        }

        sampleCt += 1

        if(G1.variableValue == G2.variableValue) {
          println("chains coupled after " + sampleCt + " samples")
          sampleMax = 0
        }
      }

      if (sampleMax != 0) {
        println("maximum iterations " + maxSamples + " exceeded")
      }

      toc("run" + irun, sampleCt)

      coupling_times(irun - 1) = sampleCt

      println("done!")

      irun += 1
    }

    println(coupling_times)
  }
}
