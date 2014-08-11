import optiml.compiler._
import optiml.library._
import optiml.shared._

object GibbsCompiler extends OptiMLApplicationCompiler with Gibbs
object GibbsInterpreter extends OptiMLApplicationInterpreter with Gibbs

trait Gibbs extends OptiMLApplication {

  def print_usage = {
    println("Usage: Gibbs <factors file> <variables file> <weights file> <edges file>")
    exit(-1)
  }

  /* Samples a variable and updates its value in the graph */
  def sampleVariable(graph: Rep[FactorGraph[FunctionFactor]], variableId: Rep[Int]) = {
    // all factors that connect to the variable
    val variableFactors = graph.variablesToFactors.apply(variableId).map(fid => graph.factors.apply(fid))

    // TODO: be domain-independent

    val allValues = variableFactors.map { factor =>
      // consider positive and negative cases
      val cases = factor.vars.map { v =>
        if (v.id == variableId && v.isPositive) pack(unit(1.0), unit(0.0))
        else if (v.id == variableId && !v.isPositive) pack(unit(0.0), unit(1.0))
        else {
          val value = graph.getVariableValue(v.id, v.isPositive)
          pack(value,value)
        }
      }

      val factorWeightValue = graph.getWeightValue(factor.weightId)
      pack(factor.evaluate(cases.map(_._1)) * factorWeightValue,
           factor.evaluate(cases.map(_._2)) * factorWeightValue)

    }

    val (positiveValues, negativeValues) = (allValues.map(_._1), allValues.map(_._2))

    val newValue = if ((random[Double] * (1.0 + exp(negativeValues.sum - positiveValues.sum))) <= 1.0) 1.0 else 0.0
    graph.updateVariableValue(variableId, newValue)
  }

  /* Samples multiple variables and updates the variable values in the graph */
  def sampleVariables(graph: Rep[FactorGraph[FunctionFactor]], variableIds: Rep[DenseVector[Int]], times: Rep[DenseVector[Tup2[Int,Long]]]) = {

    val start = time()
    val z = for (v <- variableIds) {
      sampleVariable(graph, v)
    }
    val end = time(z)

    times <<= pack(variableIds.length, end - start)
  }

  def evaluateFactor(graph: Rep[FactorGraph[FunctionFactor]], factorId: Rep[Int]): Rep[Double] = {
    val factor = graph.factors.apply(factorId)
    val factorVariableValues = factor.vars.map(fv => graph.getVariableValue(fv.id, fv.isPositive))
    factor.evaluate(factorVariableValues)
  }

  // computes the marginal probability that each factor is true
  def sampleFactors(graph: Rep[FactorGraph[FunctionFactor]], variableIds: Rep[DenseVector[Int]], factorIds: Rep[DenseVector[Int]], numSamples: Rep[Int], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
    var i = 0
    val acc = DenseVector[Double](factorIds.length, true)
    while (i < numSamples) {
      // sample all variables and update their values
      sampleVariables(graph, variableIds, times)
      acc += factorIds.map(fid => evaluateFactor(graph, fid))
      i += 1
    }
    val res = acc / numSamples
    factorIds.indices.groupByReduce[Int,Double](i => factorIds(i), i => res(i), (a,b) => a)
  }

  // TIME BREAKDOWN: sequential total is ~53s
  // sampling factors takes ~.5s per iteration --> 50s overall
  // sampleVariables is ~.17s per call * 2x per iteration --> 34s overall
  def learnWeights(graph: Rep[FactorGraph[FunctionFactor]], numIterations: Rep[Int], numSamples: Rep[Int], learningRate: Rep[Double], regularizationConstant: Rep[Double], diminishRate: Rep[Double], times: Rep[DenseVector[Tup2[Int,Long]]]): Rep[Unit] = {
    tic("initLearnWeights")
    val allVariables = graph.variables.map(_.id)
    val queryVariables = graph.variables.filter(_.isQuery).map(_.id)
    val evidenceVariables = graph.variables.filter(_.isEvidence).map(_.id)
    val evidenceValues = evidenceVariables.map(id => graph.getVariableValue(id))

    // we only learn weights for factors that are connected to evidence
    val queryFactorIds = evidenceVariables.flatMap(vid => graph.variablesToFactors.apply(vid)).distinct
    val factorWeightIds = queryFactorIds.map(fid => graph.factors.apply(fid).weightId).distinct
    val queryWeightIds = factorWeightIds.filter(wid => !graph.weights.apply(wid).isFixed)
    val weightFactorIdsMap = queryFactorIds.map(fid => graph.factors.apply(fid)).groupBy(f => f.weightId, f => f.id)
    toc("initLearnWeights", allVariables, queryVariables, evidenceValues, queryWeightIds, weightFactorIdsMap)

    println("num_iterations="+numIterations)
    println("num_samples_per_iteration="+numSamples)
    println("learning_rate="+learningRate)
    println("diminish_rate="+diminishRate)
    println("regularization_constant="+regularizationConstant)
    println("num_factors="+graph.factors.length+" num_query_factors="+queryFactorIds.length)
    println("num_weights="+graph.weights.length+" num_query_weights="+queryWeightIds.length)
    println("num_query_variables="+queryVariables.length+" num_evidence_variables="+evidenceVariables.length)

    if (queryWeightIds.length == 0) {
      println("no query weights, nothing to learn!")
    }
    else {
      untilconverged(0, minIter = numIterations, maxIter = numIterations) { i =>
        val iterLearningRate = pow(diminishRate, i) * learningRate

        println("iteration="+i+" learning_rate="+iterLearningRate)

        tic("sampleFactors")
        // compute the expectation for all factors sampling only query variables
        val conditionedEx = sampleFactors(graph, queryVariables, queryFactorIds, numSamples, times)
        // compute the expectation for all factors sampling all variables
        val unconditionedEx = sampleFactors(graph, allVariables, queryFactorIds, numSamples, times)
        toc("sampleFactors", conditionedEx, unconditionedEx)

        // compute new weights
        val weightUpdates = queryWeightIds.map { weightId =>
          val factors = weightFactorIdsMap(weightId)
          val currentWeight = graph.getWeightValue(weightId)
          def withDefaultZero(m: Rep[ForgeHashMap[Int,Double]], key: Rep[Int]) = if (m.contains(key)) m(key) else 0.0
          val weightChange = factors.map(id => (withDefaultZero(conditionedEx,id) - withDefaultZero(unconditionedEx,id))).sum
          val newWeight = currentWeight + (weightChange * iterLearningRate) * (1.0/(1.0+regularizationConstant*iterLearningRate))
          pack(weightChange, newWeight)
        }

        graph.updateWeightValues(queryWeightIds, weightUpdates.map(_._2))
        val weightChanges = weightUpdates.map(t => t._1)

        // calculate the L2 norm of the weight changes and the maximum gradient
        val gradientNorm = sqrt(sum(square(weightChanges)))
        val maxGradient = max(abs(weightChanges))
        println("gradient_norm="+gradientNorm+" max_gradient="+maxGradient)

        // reset the evidence variables to their evidence values (we changed their values by sampling them above)
        graph.updateVariableValues(evidenceVariables, evidenceValues)

        i + 1
      }
      ()
    }

  }

  def calculateMarginals(graph: Rep[FactorGraph[FunctionFactor]], numSamples: Rep[Int], variables: Rep[DenseVector[RandomVariable]], times: Rep[DenseVector[Tup2[Int,Long]]]) = {
    println("calculating marginals for num_vars="+variables.length)

    val nonEvidenceVariables = variables.filter(!_.isEvidence).map(_.id)
    // Sums of all samples values to calculate the expectation
    val sampleSums = DenseVector[Double](nonEvidenceVariables.length, true)
    // Squared sample sums to calculate running standard deviation
    val sampleSums2 = DenseVector[Double](nonEvidenceVariables.length, true)
    // We keep track of the variable values for the first 20% and last 50% of iterations.
    // We use the data for a Z-Test
    val iteration20 = (numSamples * 0.2).toInt
    val iteration50 = (numSamples * 0.5).toInt
    val sampleSumsFirst20 = DenseVector[Double](nonEvidenceVariables.length, true)
    val sampleSumsLast50 = DenseVector[Double](nonEvidenceVariables.length, true)
    val sampleSums2First20 = DenseVector[Double](nonEvidenceVariables.length, true)
    val sampleSums2Last50 = DenseVector[Double](nonEvidenceVariables.length, true)

    // TODO: Z-test for convergence
    var i = 1
    while (i <= numSamples) {
      println("iteration=" + i + "/" + numSamples)
      // samples all variables that are not evidence
      sampleVariables(graph, nonEvidenceVariables, times)

      // updated the significance statistics
      for (k <- nonEvidenceVariables.indices) {
        val sampleResult = graph.getVariableValue(nonEvidenceVariables(k))
        val sampleResultSq = sampleResult*sampleResult
        sampleSums(k) = sampleSums(k) + sampleResult
        sampleSums2(k) = sampleSums2(k) + sampleResultSq
        if (i <= iteration20) {
          sampleSumsFirst20(k) = sampleSumsFirst20(k) + sampleResult
          sampleSums2First20(k) = sampleSums2First20(k) + sampleResultSq
        }
        if (i >= iteration50) {
          sampleSumsLast50(k) = sampleSumsLast50(k) + sampleResult
          sampleSums2Last50(k) = sampleSums2Last50(k) + sampleResultSq
        }
      }

      i += 1
    }

    // calculate significane statistics
    println("calculating significant statistics...")
    val notConverged = nonEvidenceVariables.indices.map { k =>
      val meanFirst20 = sampleSumsFirst20(k) / iteration20.toDouble
      val varianceFirst20 = sampleSums2First20(k)/iteration20.toDouble - (meanFirst20 * meanFirst20)
      val meanLast50 = sampleSumsLast50(k) / iteration50.toDouble
      val varianceLast50 = sampleSums2Last50(k)/iteration20.toDouble - (meanLast50 * meanLast50)

      val varianceSum = max(varianceFirst20 + varianceLast50, 0.00001)
      val tScore = (meanFirst20 - meanLast50)/(sqrt(varianceSum))

      val notConverged95 = (abs(tScore) > 1.96)
      val notConverged90 = (abs(tScore) > 1.65)
      pack(notConverged95, notConverged90)
    }

    val (notConverged95, notConverged90) = (notConverged.map(_._1), notConverged.map(_._2))

    println("Not converged for p=0.95: "+notConverged95.count(_ == true)/nonEvidenceVariables.length)
    println("Not converged for p=0.90: "+notConverged90.count(_ == true)/nonEvidenceVariables.length)

    // generate the inference results
    nonEvidenceVariables.indices.map { k =>
      val variableId = nonEvidenceVariables(k)
      pack((variableId,
           sampleSums(k) / numSamples.toDouble,
           sqrt(numSamples * sampleSums2(k) - sampleSums(k)*sampleSums(k)) / numSamples,
           graph.getVariableValue(variableId)))
    }
  }

  def main() = {
    if (args.length < 3) print_usage

    tic("io")
    val G = readFactorGraph(args(0), args(1), args(2), args(3))
    toc("io", G)

    println("finished reading factor graph")
    println("read " + G.factors.length + " factors, " + G.variables.length + " variables, " + G.weights.length + " weights")

    // println("first 10 factors: ")
    // G.factors.apply(0::10).map(f => pack(f.id, f.vars.map(_.id), f.weightId, f.funcId)).pprint

    // println("first 10 variables: ")
    // G.variables.apply(0::10).map(v => pack(v.id, v.domain, v.value, v.isEvidence, v.isQuery)).pprint

    // println("first 10 weights: ")
    // G.weights.apply(0::10).map(w => pack(w.id, w.value, w.isFixed)).pprint

    val times1 = DenseVector[Tup2[Int,Long]](0, true)
    val times2 = DenseVector[Tup2[Int,Long]](0, true)

    tic("learnWeights", G)
    learnWeights(G, 300, 1, 0.01, 0.1, 0.95, times1)
    toc("learnWeights", G)
    writeVector(G.weights.map(w => w.id + "\t" + G.getWeightValue(w.id)), "weights.out")

    // println("first 10 learned weights: ")
    // G.weights.apply(0::10).map(w => pack(w.id, w.value, w.isFixed)).pprint

    tic("calculateMarginals", G)
    val marginals = calculateMarginals(G, 500, G.variables, times2)
    toc("calculateMarginals", marginals)
    writeVector(marginals.map(t => t._1 + "\t" + t._4.toInt + "\t" + t._2), "marginals.out")

    val totalNumSamples1 = times1.map(_._1).sum
    val totalMillis1 = times1.map(_._2).sum
    println("Learner: samples_per_sec= " + (totalNumSamples1 / (totalMillis1/1000.0)))
    val totalNumSamples2 = times2.map(_._1).sum
    val totalMillis2 = times2.map(_._2).sum
    println("Sampler: samples_per_sec= " + (totalNumSamples2 / (totalMillis2/1000.0)))
  }
}
