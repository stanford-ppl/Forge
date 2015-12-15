import optiml.compiler._
import optiml.library._
import optiml.shared._

object GibbsCompiler extends OptiMLApplicationCompiler with Gibbs
object GibbsInterpreter extends OptiMLApplicationInterpreter with Gibbs

trait Gibbs extends OptiMLApplication {

  def print_usage = {
    println("Usage: Gibbs <factors file> <variables file> <weights file> <edges file> <num samples> <num models> <num weight iterations> <num weight samples> <learning rate> <regularization constant> <diminish rate>")
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
        fatal("isequal factor function cannot contain " + nargs + " arguments")
        false
      }
    }
    else if (ffx == 4) { // ISTRUE
      if(nargs == 1) {
        args(0)
      }
      else {
        fatal("istrue factor function cannot contain " + nargs + " arguments")
        false
      }
    }
    else {
      fatal("invalid factor function " + ffx)
      false
    }
  }

  def readFVUnder(G: Rep[FactorGraph], f: Rep[Int], v: Rep[Int], x: Boolean, idx: Rep[Int]): Rep[Boolean] = {
    val ie = G.f2v.ngbrEdges(f).apply(idx)
    val iv = G.f2v.edges.apply(ie)

    val vv = if(v == iv) x else G.variableValue.apply(iv)
    if (G.edgeIsPositiveF2V.apply(ie)) vv else !vv
  }

  def readFV(G: Rep[FactorGraph], f: Rep[Int], idx: Rep[Int]): Rep[Boolean] = {
    val ie = G.f2v.ngbrEdges(f).apply(idx)
    val iv = G.f2v.edges.apply(ie)

    val vv = G.variableValue.apply(iv)
    if (G.edgeIsPositiveF2V.apply(ie)) vv else !vv
  }

  def evalFactorUnder(G: Rep[FactorGraph], f: Rep[Int], v: Rep[Int], x: Boolean): Rep[Double] = {
    val nvars = G.f2v.ngbrEdges(f).length
    val ffx = G.factorFunction.apply(f)

    val z = evalFFX(ffx, nvars, { idx => readFVUnder(G, f, v, x, idx) })
    // get the weight
    val w = G.weightValue.apply(G.factorWeightIdx.apply(f))
    // finally, return the weight times the result
    if (z) w else 0.0
  }

  def evalFactorDiff(G: Rep[FactorGraph], f: Rep[Int]): Rep[Double] = {
    val nvars = G.f2v.ngbrEdges(f).length
    val ffx = G.factorFunction.apply(f)

    val z = evalFFX(ffx, nvars, { idx => readFV(G, f, idx) })
    // finally, return the weight times the result
    if (z) 1.0 else 0.0
  }

  def sampleVariable(G: Rep[FactorGraph], v: Rep[Int]): Rep[Boolean] = {
    val dw: Rep[Double] = G.v2f.ngbrNodes(v).map({ f =>
      val w0 = evalFactorUnder(G, f, v, false)
      val w1 = evalFactorUnder(G, f, v, true)
      w1 - w0
    }).sum

    val newValue: Rep[Boolean] = ((random[Double] * (1.0 + exp(-dw))) <= 1.0)
    G.variableValue(v) = newValue
    newValue
  }

  def sampleFactors(G: Rep[FactorGraph], vars: Rep[DenseVector[Int]], factors: Rep[DenseVector[Int]], numSamples: Rep[Int]): Rep[DenseVector[Double]] = {
    var sampleCt = 0
    var acc = DenseVector.zeros(factors.length)
    while (sampleCt < numSamples) {
      for (vv <- (0::vars.length)) {
        sampleVariable(G, vars(vv))
        ()
      }
      acc = acc + factors.map(fid => evalFactorDiff(G, fid))
      sampleCt += 1
    }
    acc / numSamples
  }

  def learnWeights(G: Rep[FactorGraph], numIterations: Rep[Int], numSamples: Rep[Int], learningRate: Rep[Double], regularizationConstant: Rep[Double], diminishRate: Rep[Double]): Rep[FactorGraph] = {

    println("")
    println("learning weights...")

    // determine which weights are connected to evidence
    val queryFactorIds = (0::G.numVariables).filter(vid => G.variableIsEvidence.apply(vid)).flatMap(vid => G.v2f.ngbrNodes(vid)).distinct
    val factorWeightIds = queryFactorIds.map(fid => G.factorWeightIdx.apply(fid)).distinct
    val queryWeightIds = factorWeightIds.filter(wid => !G.weightIsFixed.apply(wid))
    val weightFactorIdsMap = (0::queryFactorIds.length).groupBy(q => G.factorWeightIdx.apply(queryFactorIds(q)), q => q)

    if(queryWeightIds.length == 0) {
      println("no weights to learn!")
      println("done!")
      println("")
      G
    }
    else {
      println("learning " + queryWeightIds.length + " weights from " + queryFactorIds.length + " factors")
      println("using " + numIterations + " iterations of " + numSamples + " each")

      val gmc = G.mutable()
      val gmu = G.mutable()

      var iterct = 0
      var iterLearningRate: Rep[Double] = learningRate
      while(iterct < numIterations) {
        // compute the expectation for all factors sampling only query variables
        val conditionedEx = sampleFactors(gmc, G.nonEvidenceVariables, queryFactorIds, numSamples)
        // compute the expectation for all factors sampling all variables
        val unconditionedEx = sampleFactors(gmu, (0::G.numVariables), queryFactorIds, numSamples)

        // compute new weights
        val gmcwv = gmc.weightValue
        val gmuwv = gmu.weightValue
        for(q <- (0::queryWeightIds.length)) {
          val wid: Rep[Int] = queryWeightIds(q)
          val dw: Rep[Double] = weightFactorIdsMap(wid).map(q => conditionedEx(q) - unconditionedEx(q)).sum
          gmcwv(wid) = gmcwv(wid) * (1.0 / (1.0 + regularizationConstant * iterLearningRate)) + (dw * iterLearningRate)
          gmuwv(wid) = gmcwv(wid)
        }

        // reset evidence variables
        // val gmvv = gm.variableValue
        // for(vid <- 0::gm.numVariables) {
        //   if(gm.variableIsEvidence.apply(vid)) {
        //     gmvv(vid) = G.variableValue.apply(vid)
        //   }
        // }

        iterLearningRate = iterLearningRate * diminishRate
        iterct += 1
      }

      println("done!")
      println("")

      gmc
    }
  }

  def main() = {
    if (args.length < 11) print_usage

    tic("io")
    val numSamples = args(4).toInt
    val numModels = args(5).toInt
    val numSamplesPerModel = numSamples / numModels
    val numIterationsWL = args(6).toInt
    val numSamplesWL = args(7).toInt
    val learningRate = args(8).toDouble
    val regularizationConstant = args(9).toDouble
    val diminishRate = args(10).toDouble
    val G = readFactorGraph(args(0), args(1), args(2), args(3))
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

    val Gwl = learnWeights(G, numIterationsWL, numSamplesWL, learningRate, regularizationConstant, diminishRate) 

    val outWeights: Rep[DenseVector[String]] = (0::Gwl.numWeights).map({iw => 
      "" + iw + "\t" + Gwl.weightValue.apply(iw) + "\t" + Gwl.weightIsFixed.apply(iw)
    })
    writeVector(outWeights, "weights.out")

    println("")
    println("replicating the graph...")
    val GR = replicate(Gwl)
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
