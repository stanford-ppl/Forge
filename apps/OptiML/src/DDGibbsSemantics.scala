import optiml.compiler._
import optiml.library._
import optiml.shared._

object DDGibbsLinearCompiler extends OptiMLApplicationCompiler with DDGibbsLinear
object DDGibbsLinearInterpreter extends OptiMLApplicationInterpreter with DDGibbsLinear

object DDGibbsRatioCompiler extends OptiMLApplicationCompiler with DDGibbsRatio
object DDGibbsRatioInterpreter extends OptiMLApplicationInterpreter with DDGibbsRatio

object DDGibbsLogicalCompiler extends OptiMLApplicationCompiler with DDGibbsLogical
object DDGibbsLogicalInterpreter extends OptiMLApplicationInterpreter with DDGibbsLogical

trait DDGibbsLinear extends DDGibbsSemantics {
  override def semantic_function(n: Rep[Double]): Rep[Double] = {
    n
  }
}

trait DDGibbsRatio extends DDGibbsSemantics {
  override def semantic_function(n: Rep[Double]): Rep[Double] = {
    log(n + 1.0) / log(2.0)
  }
}

trait DDGibbsLogical extends DDGibbsSemantics {
  override def semantic_function(n: Rep[Double]): Rep[Double] = {
    if (n > 0.5) {
      1.0
    }
    else {
      0.0
    }
  }
}

trait DDGibbsSemantics extends OptiMLApplication {

  def semantic_function(n: Rep[Double]): Rep[Double] = {
    println("error: invalid semantic function")
    exit(-1)
    0.0
  }

  def print_usage = {
    println("Usage: DDGibbsSemantics <factors file> <variables file> <weights file> <edges file> <output marginals> <num samples>")
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
      if (args(0)) {
        1.0
      }
      else {
        0.0
      }
    }
    else if (ffx == 6) { //RATIO
      val bhead = args(nargs - 1)
      var acc = 0.0
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
        semantic_function(acc)
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

  def main() = {
    if (args.length < 6) print_usage

    tic("io")
    val outPath = args(4)
    val numSamples = args(5).toInt
    val G = readDDFactorGraph(args(0), args(1), args(2), args(3))
    toc("io", G)

    println("finished reading factor graph")
    println("read:")
    println("  " + G.numFactors + " factors")
    println("  " + G.numVariables + " variables")
    println("  " + G.numWeights + " weights")
    println("  " + G.numEdges + " edges")
    println("  " + G.nonEvidenceVariables.length + " non-evidence variables")

    println("")
    println("done!")

    println("")
    println("estimating marginals (numsamples = " + numSamples + ")")

    tic()

    val graph = G.mutableVariables()
    val marginalsAcc = DenseVector.zeros(graph.nonEvidenceVariables.length).mutable()
    var sampleCt = 0
    while (sampleCt < numSamples) {
      for (iv <- 0::graph.nonEvidenceVariables.length) {
        val v = graph.nonEvidenceVariables.apply(iv)
        marginalsAcc(iv) = marginalsAcc(iv) + (if (sampleVariable(graph, v)) 1.0 else 0.0)
      }
      sampleCt += 1
      println("sampling " + sampleCt + "/" + numSamples)
    }
    
    val marginals = marginalsAcc / numSamples

    toc(marginals)

    println("done!")

    writeVector(marginals, outPath)
  }
}
