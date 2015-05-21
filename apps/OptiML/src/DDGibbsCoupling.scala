import optiml.compiler._
import optiml.library._
import optiml.shared._

object DDGibbsCouplingCompiler extends OptiMLApplicationCompiler with DDGibbsCoupling
object DDGibbsCouplingInterpreter extends OptiMLApplicationInterpreter with DDGibbsCoupling

trait DDGibbsCoupling extends OptiMLApplication {

  def print_usage = {
    println("Usage: DDGibbsCoupling <factors file> <variables file> <weights file> <edges file> <num samples> <num models> <num weight iterations> <num weight samples> <learning rate> <regularization constant> <diminish rate>")
    exit(-1)
  }

  def evalFFX(ffx: Rep[Int], nargs: Rep[Int], args: Rep[Int] => Rep[Boolean]): Rep[Boolean] = {
    if(ffx == 0) { // IMPLY
      var acc = args(nargs - 1)
      var idx = 0
      while ((idx < nargs - 1)&&(acc == false)) {
        if (args(idx) == false) {
          acc = true
        }
        idx += 1
      }
      acc
    }
    else if (ffx == 1) { // OR
      var acc = false
      var idx = 0
      while ((idx < nargs)&&(acc == false)) {
        acc = args(idx)
        idx += 1
      }
      acc
    }
    else if (ffx == 2) { // AND
      var acc = true
      var idx = 0
      while ((idx < nargs)&&(acc == true)) {
        acc = args(idx)
        idx += 1
      }
      acc
    }
    else if (ffx == 3) { // EQUAL
      if(nargs == 2) {
        (args(0) == args(1))
      } 
      else {
        println("error: isequal factor function cannot contain " + nargs + " arguments")
        exit(-1)
        false
      }
    }
    else if (ffx == 4) { // ISTRUE
      if(nargs == 1) {
        args(0)
      }
      else {
        println("error: istrue factor function cannot contain " + nargs + " arguments")
        exit(-1)
        false
      }
    }
    else {
      println("error: invalid factor function " + ffx)
      exit(-1)
      false
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
    if (z) {
      w
    }
    else {
      0.0
    }
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
    if (args.length < 5) print_usage

    tic("io")
    var maxSamples = args(4).toInt
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

    println("")
    println("done!")

    println("")
    println("creating two mutable copies of the graph...")
    val G1 = randomizeVariables(G)
    val G2 = randomizeVariables(G)
    println("done!")

    println("")
    println("estimating coupling time")

    tic()

    var sampleCt = 0
    while (sampleCt < maxSamples) {
      for (iv <- 0::G.nonEvidenceVariables.length) {
        val v = G.nonEvidenceVariables.apply(iv)
        val rd: Rep[Double] = random[Double]
        sampleVariable(G1, v, rd)
        sampleVariable(G2, v, rd)
      }

      if(G1.variableValue == G2.variableValue) {
        println("chains coupled after " + sampleCt + " samples")
        maxSamples = 0
      }
    }

    if (maxSamples != 0) {
      println("maximum iterations " + maxSamples + " exceeded")
    }

    toc(sampleCt)

    println("done!")
  }
}
