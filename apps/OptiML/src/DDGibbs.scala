import optiml.compiler._
import optiml.library._
import optiml.shared._

object DDGibbsCompiler extends OptiMLApplicationCompiler with DDGibbs
object DDGibbsInterpreter extends OptiMLApplicationInterpreter with DDGibbs

trait DDGibbs extends OptiMLApplication {

  def print_usage = {
    println("Usage: DDGibbs <factors file> <variables file> <weights file> <edges file> <num samples>")
    exit(-1)
  }

  def evalFFXUnder(G: Rep[DDFactorGraph], f: Rep[Int], v: Rep[Int], x: Rep[Boolean]): Rep[Boolean] = {
    val nvars = G.f2v.ngbrEdges(f).length
    val ffx = G.factorFunction.apply(f)

    if(ffx == 0) { // IMPLY
      var acc = readFVUnder(G, f, v, x, nvars - 1)
      var idx = 0
      while ((idx < nvars - 1)&&(acc == false)) {
        if (readFVUnder(G, f, v, x, idx) == false) {
          acc = true
        }
        idx += 1
      }
      acc
    }
    else if (ffx == 1) { // OR
      var acc = false
      var idx = 0
      while ((idx < nvars)&&(acc == false)) {
        acc = readFVUnder(G, f, v, x, idx)
        idx += 1
      }
      acc
    }
    else if (ffx == 2) { // AND
      var acc = true
      var idx = 0
      while ((idx < nvars)&&(acc == true)) {
        acc = readFVUnder(G, f, v, x, idx)
        idx += 1
      }
      acc
    }
    else if (ffx == 3) { // EQUAL
      if(nvars == 2) {
        (readFVUnder(G, f, v, x, 0) == readFVUnder(G, f, v, x, 1))
      } 
      else {
        println("error: isequal factor function cannot contain " + args.length + " arguments")
        exit(-1)
        false
      }
    }
    else if (ffx == 4) { // ISTRUE
      if(nvars == 1) {
        readFVUnder(G, f, v, x, 0)
      } 
      else {
        println("error: istrue factor function cannot contain " + args.length + " arguments")
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
    val z = evalFFXUnder(G, f, v, x)
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
    if (args.length < 5) print_usage

    tic("io")
    val numSamples = args(4).toInt
    val numModels = args(5).toInt
    val numSamplesPerModel = numSamples / numModels
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
    println("replicating the graph...")
    val GR = replicate(G)
    println("done!")

    println("")
    println("estimating marginals (numsamples = " + numSamples + ")")

    tic()

    val marginals = sum(0,numModels) { imodel =>
      val graph = GR.local.mutableVariables()
      val marginalsAcc = DenseVector.zeros(graph.nonEvidenceVariables.length).mutable()
      var sampleCt = 0
      while (sampleCt < numSamplesPerModel) {
        for (iv <- 0::graph.nonEvidenceVariables.length) {
          val v = graph.nonEvidenceVariables.apply(iv)
          marginalsAcc(iv) = marginalsAcc(iv) + (if (sampleVariable(graph, v)) 1.0 else 0.0)
        }
        sampleCt += 1
        //println("sampling " + sampleCt + "/" + numSamples)
      }
      marginalsAcc
    } / numSamples

    toc(marginals)

    println("done!")

    val outMarginals: Rep[DenseVector[String]] = (0::G.nonEvidenceVariables.length).map({iv => 
      "" + G.nonEvidenceVariables.apply(iv) + "\t" + marginals(iv)
    })
    writeVector(outMarginals, "marginals.out")
  }
}
