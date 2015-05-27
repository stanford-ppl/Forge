import optiml.compiler._
import optiml.library._
import optiml.shared._

object DDGibbsErrLinearCompiler extends OptiMLApplicationCompiler with DDGibbsErrLinear
object DDGibbsErrLinearInterpreter extends OptiMLApplicationInterpreter with DDGibbsErrLinear

object DDGibbsErrRatioCompiler extends OptiMLApplicationCompiler with DDGibbsErrRatio
object DDGibbsErrRatioInterpreter extends OptiMLApplicationInterpreter with DDGibbsErrRatio

object DDGibbsErrLogicalCompiler extends OptiMLApplicationCompiler with DDGibbsErrLogical
object DDGibbsErrLogicalInterpreter extends OptiMLApplicationInterpreter with DDGibbsErrLogical

trait DDGibbsErrLinear extends DDGibbsErr {
  override def semantic_function(n: Rep[Double]): Rep[Double] = {
    n
  }
}

trait DDGibbsErrRatio extends DDGibbsErr {
  override def semantic_function(n: Rep[Double]): Rep[Double] = {
    log(n + 1.0) / log(2.0)
  }
}

trait DDGibbsErrLogical extends DDGibbsErr {
  override def semantic_function(n: Rep[Double]): Rep[Double] = {
    if (n > 0.5) {
      1.0
    }
    else {
      0.0
    }
  }
}

trait DDGibbsErr extends OptiMLApplication {

  def semantic_function(n: Rep[Double]): Rep[Double] = {
    println("error: invalid semantic function")
    exit(-1)
    0.0
  }

  def print_usage = {
    println("Usage: DDGibbsErr <factors file> <variables file> <weights file> <edges file> <input marginals> <output file> <num samples> <num models>")
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
      if (nargs == 1) {
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
    else if (ffx == 8) { //RATIO
      val bhead = args(nargs - 1)
      var acc = 0.0
      var idx = 0
      while (idx < nargs - 1) {
        if (args(idx)) {
          acc += 1.0
        }
        idx += 1
      }
      if (nargs == 1) {
        if (bhead) {
          1.0
        }
        else {
          -1.0
        }
      }
      else {
        if (bhead) {
          semantic_function(acc)
        }
        else {
          -semantic_function(acc)
        }
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
    w * z
  }

  def sampleVariable(G: Rep[DDFactorGraph], v: Rep[Int]): Rep[Boolean] = {
    val dw: Rep[Double] = G.v2f.ngbrNodes(v).map({ f =>
      val w0 = evalFactorUnder(G, f, v, false)
      val w1 = evalFactorUnder(G, f, v, true)
      w1 - w0
    }).sum

    val newValue: Rep[Boolean] = ((random[Double] * (1.0 + exp(-dw))) <= 1.0)
    G.variableValue(v) = newValue
    newValue
  }

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
    if (args.length < 8) print_usage

    tic("io")
    val marginalsPath = args(4)
    val outputPath = args(5)
    val numSamples = args(6).toInt
    val numModels = args(7).toInt
    val G = readDDFactorGraph(args(0), args(1), args(2), args(3))
    toc("io", G)

    println("finished reading factor graph")
    println("read:")
    println("  " + G.numFactors + " factors")
    println("  " + G.numVariables + " variables")
    println("  " + G.numWeights + " weights")
    println("  " + G.numEdges + " edges")
    println("  " + G.nonEvidenceVariables.length + " non-evidence variables")

    val goldMarginals = readVector(marginalsPath)

    println("finished reading marginals")
    println("read " + goldMarginals.length + " marginals")

    println("")
    println("done!")

    println("")
    println("estimating marginals (numsamples = " + numSamples + ")")

    tic()

    val graphs = (0::numModels) map { i => randomizeVariables(G) }
    val marginalsAcc = DenseMatrix.zeros(numModels, goldMarginals.length).mutable()
    val errs = DenseVector[Double](numSamples, false)

    var sampleCt = 0
    while (sampleCt < numSamples) {
      for (iv <- 0::goldMarginals.length) {
        for (im <- 0::numModels) {
          val v = graphs(im).nonEvidenceVariables.apply(iv)
          marginalsAcc(im, iv) = marginalsAcc(im, iv) + (if (sampleVariable(graphs(im), v)) 1.0 else 0.0)
        }
      }

      errs(sampleCt) = sum(0, numModels) { im =>
        (sum(0, goldMarginals.length) { iv =>
          val dd: Rep[Double] = (marginalsAcc(im, iv) / (sampleCt + 1.0)) - goldMarginals(iv) 
          dd * dd
        })
      } / numModels

      sampleCt += 1
      println("sampling " + sampleCt + "/" + numSamples)
    }

    toc(errs)

    println("done!")

    writeVector(errs, outputPath)
  }
}
