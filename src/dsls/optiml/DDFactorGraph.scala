package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

// DeepDive/DimmWitted factor graph
trait DDFactorGraphOps {
  this: OptiMLDSL =>

  def importAllDDFactorGraphOps() {
    importDDFactorGraphOps()
    importDDFactorGraphFileOps()
    importDDFactorGraphLoaderOps()
    importDDFactorGraphReplicateHackOps()
  }

  def importDDFactorGraphOps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val CSRGraph = lookupTpe("CSRGraph")
    val DDFactorGraph = tpe("DDFactorGraph")

    data(DDFactorGraph,
      ("_v2f", CSRGraph), 
      ("_f2v", CSRGraph), 
      ("_weightValue", DenseVector(MDouble)), 
      ("_weightIsFixed", DenseVector(MBoolean)),
      ("_variableValue", DenseVector(MBoolean)),
      ("_variableIsEvidence", DenseVector(MBoolean)),
      ("_factorWeightIdx", DenseVector(MInt)),
      ("_factorFunction", DenseVector(MInt)),
      ("_edgeIsPositiveF2V", DenseVector(MBoolean)),
      ("_nonEvidenceVariables", DenseVector(MInt))
    )

    static (DDFactorGraph) ("apply", Nil, MethodSignature(List(
      ("v2f", CSRGraph), 
      ("f2v", CSRGraph), 
      ("weightValue", DenseVector(MDouble)), 
      ("weightIsFixed", DenseVector(MBoolean)),
      ("variableValue", DenseVector(MBoolean)),
      ("variableIsEvidence", DenseVector(MBoolean)),
      ("factorWeightIdx", DenseVector(MInt)),
      ("factorFunction", DenseVector(MInt)),
      ("edgeIsPositiveF2V", DenseVector(MBoolean)),
      ("nonEvidenceVariables", DenseVector(MInt))
    ), DDFactorGraph)) implements
      allocates(DDFactorGraph, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5}, ${$6}, ${$7}, ${$8}, ${$9})

    val DDFactorGraphOps = withTpe(DDFactorGraph)
    DDFactorGraphOps {
      infix ("v2f") (Nil :: CSRGraph) implements getter(0, "_v2f")
      infix ("f2v") (Nil :: CSRGraph) implements getter(0, "_f2v")

      infix ("numVariables") (Nil :: MInt) implements composite ${
        ($self).v2f.numNodes
      }

      infix ("numFactors") (Nil :: MInt) implements composite ${
        ($self).f2v.numNodes
      }

      infix ("numEdges") (Nil :: MInt) implements composite ${
        ($self).v2f.numEdges
      }

      infix ("numWeights") (Nil :: MInt) implements composite ${
        ($self).weightValue.length
      }

      infix ("weightValue") (Nil :: DenseVector(MDouble)) implements getter(0, "_weightValue")

      infix ("weightIsFixed") (Nil :: DenseVector(MBoolean)) implements getter(0, "_weightIsFixed")

      infix ("variableValue") (Nil :: DenseVector(MBoolean)) implements getter(0, "_variableValue")

      infix ("variableIsEvidence") (Nil :: DenseVector(MBoolean)) implements getter(0, "_variableIsEvidence")

      infix ("factorWeightIdx") (Nil :: DenseVector(MInt)) implements getter(0, "_factorWeightIdx")

      infix ("factorFunction") (Nil :: DenseVector(MInt)) implements getter(0, "_factorFunction")

      infix ("edgeIsPositiveF2V") (Nil :: DenseVector(MBoolean)) implements getter(0, "_edgeIsPositiveF2V")

      infix ("nonEvidenceVariables") (Nil :: DenseVector(MInt)) implements getter(0, "_nonEvidenceVariables")

      infix ("mutable") (Nil :: DDFactorGraph) implements composite ${
        DDFactorGraph(
          $self.v2f,
          $self.f2v,
          $self.weightValue.mutable(),
          $self.weightIsFixed,
          $self.variableValue.mutable(),
          $self.variableIsEvidence,
          $self.factorWeightIdx,
          $self.factorFunction,
          $self.edgeIsPositiveF2V,
          $self.nonEvidenceVariables
        )
      }

      infix ("mutableWeights") (Nil :: DDFactorGraph) implements composite ${
        DDFactorGraph(
          $self.v2f,
          $self.f2v,
          $self.weightValue.mutable(),
          $self.weightIsFixed,
          $self.variableValue,
          $self.variableIsEvidence,
          $self.factorWeightIdx,
          $self.factorFunction,
          $self.edgeIsPositiveF2V,
          $self.nonEvidenceVariables
        )
      }

      infix ("mutableVariables") (Nil :: DDFactorGraph) implements composite ${
        DDFactorGraph(
          $self.v2f,
          $self.f2v,
          $self.weightValue,
          $self.weightIsFixed,
          $self.variableValue.mutable(),
          $self.variableIsEvidence,
          $self.factorWeightIdx,
          $self.factorFunction,
          $self.edgeIsPositiveF2V,
          $self.nonEvidenceVariables
        )
      }

      infix ("deepcopy") (Nil :: DDFactorGraph) implements composite ${
        DDFactorGraph(
          $self.v2f.deepcopy,
          $self.f2v.deepcopy,
          $self.weightValue.Clone,
          $self.weightIsFixed.Clone,
          $self.variableValue.Clone,
          $self.variableIsEvidence.Clone,
          $self.factorWeightIdx.Clone,
          $self.factorFunction.Clone,
          $self.edgeIsPositiveF2V.Clone,
          $self.nonEvidenceVariables.Clone
        )
      }
    }
  }

  def importDDFactorGraphFileOps() {
    val DDFGFWeight = tpe("DDFGFWeight")
    val DDFGFVariable = tpe("DDFGFVariable")
    val DDFGFFactor = tpe("DDFGFFactor")
    val DDFGFEdge = tpe("DDFGFEdge")

    data(DDFGFWeight,
      ("_weightId", MInt), 
      ("_isFixed", MBoolean), 
      ("_initialValue", MDouble)
    )

    static (DDFGFWeight) ("apply", Nil, (MInt, MBoolean, MDouble) :: DDFGFWeight) implements
      allocates(DDFGFWeight, ${$0}, ${$1}, ${$2})

    val DDFGFWeightOps = withTpe(DDFGFWeight)
    DDFGFWeightOps {
      infix ("weightId") (Nil :: MInt) implements getter(0, "_weightId")
      infix ("isFixed") (Nil :: MBoolean) implements getter(0, "_isFixed")
      infix ("initialValue") (Nil :: MDouble) implements getter(0, "_initialValue")
    }


    data(DDFGFVariable,
      ("_variableId", MInt), 
      ("_isEvidence", MBoolean), 
      ("_initialValue", MDouble),
      ("_dataType", MInt),
      ("_edgeCount", MInt),
      ("_cardinality", MInt)
    )

    static (DDFGFVariable) ("apply", Nil, MethodSignature(List(MInt, MBoolean, MDouble, MInt, MInt, MInt), DDFGFVariable)) implements
      allocates(DDFGFVariable, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})

    val DDFDFVariableOps = withTpe(DDFGFVariable)
    DDFDFVariableOps {
      infix ("variableId") (Nil :: MInt) implements getter(0, "_variableId")
      infix ("isEvidence") (Nil :: MBoolean) implements getter(0, "_isEvidence")
      infix ("initialValue") (Nil :: MDouble) implements getter(0, "_initialValue")
      infix ("dataType") (Nil :: MInt) implements getter(0, "_dataType")
      infix ("edgeCount") (Nil :: MInt) implements getter(0, "_edgeCount")
      infix ("cardinality") (Nil :: MInt) implements getter(0, "_cardinality")
    }


    data(DDFGFFactor,
      ("_factorId", MInt), 
      ("_weightId", MInt), 
      ("_factorFunction", MInt),
      ("_edgeCount", MInt)
    )

    static (DDFGFFactor) ("apply", Nil, (MInt, MInt, MInt, MInt) :: DDFGFFactor) implements
      allocates(DDFGFFactor, ${$0}, ${$1}, ${$2}, ${$3})

    val DDFGFFactorOps = withTpe(DDFGFFactor)
    DDFGFFactorOps {
      infix ("factorId") (Nil :: MInt) implements getter(0, "_factorId")
      infix ("weightId") (Nil :: MInt) implements getter(0, "_weightId")
      infix ("factorFunction") (Nil :: MInt) implements getter(0, "_factorFunction")
      infix ("edgeCount") (Nil :: MInt) implements getter(0, "_edgeCount")
    }


    data(DDFGFEdge,
      ("_variableId", MInt), 
      ("_factorId", MInt), 
      ("_position", MInt),
      ("_isPositive", MBoolean),
      ("_equalPredicate", MInt)
    )

    static (DDFGFEdge) ("apply", Nil, (MInt, MInt, MInt, MBoolean, MInt) :: DDFGFEdge) implements
      allocates(DDFGFEdge, ${$0}, ${$1}, ${$2}, ${$3}, ${$4})

    val DDFGFEdgeOps = withTpe(DDFGFEdge)
    DDFGFEdgeOps {
      infix ("variableId") (Nil :: MInt) implements getter(0, "_variableId")
      infix ("factorId") (Nil :: MInt) implements getter(0, "_factorId")
      infix ("position") (Nil :: MInt) implements getter(0, "_position")
      infix ("isPositive") (Nil :: MBoolean) implements getter(0, "_isPositive")
      infix ("equalPredicate") (Nil :: MInt) implements getter(0, "_equalPredicate")
    }
  }

  def importDDFactorGraphLoaderOps() {
    val DenseVector = lookupTpe("DenseVector")
    val CSRGraph = lookupTpe("CSRGraph")
    val DDFactorGraph = lookupTpe("DDFactorGraph")
    val DDFGFWeight = lookupTpe("DDFGFWeight")
    val DDFGFVariable = lookupTpe("DDFGFVariable")
    val DDFGFFactor = lookupTpe("DDFGFFactor")
    val DDFGFEdge = lookupTpe("DDFGFEdge")

    val IO = grp("IO")

    compiler (IO) ("ddfg_read_weights", Nil, ("path", MString) :: DenseVector(DDFGFWeight)) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[DDFGFWeight](0, true)
      while (dis.available() > 0) {
        val weightId = dis.readLong().toInt
        val isFixed = dis.readBoolean()
        val initialValue = dis.readDouble()   

        out <<= DDFGFWeight(weightId, isFixed, initialValue)
      }
      dis.fclose()
      out.unsafeImmutable
    }

    compiler (IO) ("ddfg_read_variables", Nil, ("path",MString) :: DenseVector(DDFGFVariable)) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[DDFGFVariable](0, true)
      while (dis.available() > 0) {
        val variableId = dis.readLong().toInt
        val isEvidence = false //dis.readBoolean()
        val initialValue = dis.readDouble()
        val dataType = dis.readShort().toInt
        val edgeCount = dis.readLong().toInt
        val cardinality = dis.readLong().toInt
        
        out <<= DDFGFVariable(variableId, isEvidence, initialValue, dataType, edgeCount, cardinality)
      }
      dis.fclose()
      out.unsafeImmutable
    }

    compiler (IO) ("ddfg_read_edges", Nil, ("path",MString) :: DenseVector(DDFGFEdge)) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[DDFGFEdge](0, true)
      while (dis.available() > 0) {
        val variableId = dis.readLong().toInt
        val factorId = dis.readLong().toInt
        val position = dis.readLong().toInt
        val isPositive = dis.readBoolean()
        val equalPredicate = dis.readLong().toInt
        
        out <<= DDFGFEdge(variableId, factorId, position, isPositive, equalPredicate)
      }
      dis.fclose()
      out.unsafeImmutable
    }

    compiler (IO) ("ddfg_read_factors", Nil, ("path",MString) :: DenseVector(DDFGFFactor)) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[DDFGFFactor](0, true)
      while (dis.available() > 0) {
        val factorId = dis.readLong().toInt
        val weightId = dis.readLong().toInt
        val factorFunction = dis.readShort().toInt
        val edgeCount = dis.readLong().toInt

        out <<= DDFGFFactor(factorId, weightId, factorFunction, edgeCount)
      }
      dis.fclose()
      out.unsafeImmutable
    }

    /* temporary cumulative sum */
    compiler (IO) ("ddfg_util_cumsum", Nil, DenseVector(MInt) :: DenseVector(MInt)) implements composite ${
      val rv = ((0::($0.length + unit(1))) map { i => unit(0) }).mutable
      rv(0) = unit(0)

      var idx = unit(0)
      while(idx < $0.length) {
        rv(idx + unit(1)) = rv(idx) + $0(idx)
        idx += unit(1)
      }

      rv.unsafeImmutable
    }

    // ("v2f", CSRGraph), 
    // ("f2v", CSRGraph), 
    // ("weightValue", DenseVector(MDouble)), 
    // ("weightIsFixed", DenseVector(MBoolean)),
    // ("variableValue", DenseVector(MBoolean)),
    // ("variableIsEvidence", DenseVector(MBoolean)),
    // ("factorWeightIdx", DenseVector(MInt)),
    // ("factorFunction", DenseVector(MInt)),
    // ("edgeIsPositiveF2V", DenseVector(MBoolean)),
    // ("nonEvidenceVariables", DenseVector(MInt))

    direct (IO) ("readDDFactorGraph", Nil, MethodSignature(List(("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("edgesPath", MString)), DDFactorGraph)) implements composite ${
      val factorsRaw = ddfg_read_factors($factorsPath)
      val variablesRaw = ddfg_read_variables($variablesPath)
      val weightsRaw = ddfg_read_weights($weightsPath)
      val edgesRaw = ddfg_read_edges($edgesPath)

      val v2fEdgeCounts = (variablesRaw map (v => unit(0))).mutable
      val f2vEdgeCounts = (factorsRaw map (v => unit(0))).mutable
      var ecidx = unit(0)
      while (ecidx < edgesRaw.length) {
        val e = edgesRaw(ecidx)
        v2fEdgeCounts(e.variableId) = v2fEdgeCounts(e.variableId) + unit(1)
        f2vEdgeCounts(e.factorId) = f2vEdgeCounts(e.factorId) + unit(1)
        ecidx += unit(1)
      }

      // val f2vEdgeCounts = factorsRaw map (f => f.edgeCount)

      val v2fnodes = ddfg_util_cumsum(v2fEdgeCounts)
      val f2vnodes = ddfg_util_cumsum(f2vEdgeCounts)

      val v2fEdgeIdx = (variablesRaw map (v => unit(0))).mutable

      val v2fedges = DenseVector[Int](edgesRaw.length, unit(true))
      val f2vedges = DenseVector[Int](edgesRaw.length, unit(true))
      val weightValue = DenseVector[Double](weightsRaw.length, unit(true))
      val weightIsFixed = DenseVector[Boolean](weightsRaw.length, unit(true))
      val variableValue = DenseVector[Boolean](variablesRaw.length, unit(true))
      val variableIsEvidence = DenseVector[Boolean](variablesRaw.length, unit(true))
      val factorWeightIdx = DenseVector[Int](factorsRaw.length, unit(true))
      val factorFunction = DenseVector[Int](factorsRaw.length, unit(true))
      val edgeIsPositiveF2V = DenseVector[Boolean](edgesRaw.length, unit(true))

      val nonEvidenceVariables = variablesRaw.filter(v => !v.isEvidence).map(v => v.variableId).sort()

      var widx = unit(0)
      while (widx < weightsRaw.length) {
        val w = weightsRaw(widx)
        weightValue(w.weightId) = w.initialValue
        weightIsFixed(w.weightId) = w.isFixed
        widx += unit(1)
      }

      var vidx = unit(0)
      while (vidx < variablesRaw.length) {
        val v = variablesRaw(vidx)
        variableValue(v.variableId) = (v.initialValue >= unit(0.5)) 
        variableIsEvidence(v.variableId) = v.isEvidence
        vidx += unit(1)
      }

      var fidx = unit(0)
      while (fidx < factorsRaw.length) {
        val f = factorsRaw(fidx)
        factorWeightIdx(f.factorId) = f.weightId
        factorFunction(f.factorId) = f.factorFunction

        if(f2vEdgeCounts(f.factorId) != f.edgeCount) {
          println("edge mismatch error: " + f2vEdgeCounts(f.factorId) + " != " + f.edgeCount)
          exit(-1)
        }

        fidx += unit(1)
      }

      var eidx = unit(0)
      while (eidx < edgesRaw.length) {
        val e = edgesRaw(eidx)
        f2vedges(f2vnodes(e.factorId) + e.position) = e.variableId
        v2fedges(v2fnodes(e.variableId) + v2fEdgeIdx(e.variableId)) = e.factorId
        v2fEdgeIdx(e.variableId) = v2fEdgeIdx(e.variableId) + unit(1)
        edgeIsPositiveF2V(f2vnodes(e.factorId) + e.position) = e.isPositive
        eidx += unit(1)
      }

      DDFactorGraph(
        CSRGraph(v2fnodes.unsafeImmutable, v2fedges.unsafeImmutable),
        CSRGraph(f2vnodes.unsafeImmutable, f2vedges.unsafeImmutable),
        weightValue.unsafeImmutable,
        weightIsFixed.unsafeImmutable,
        variableValue.unsafeImmutable,
        variableIsEvidence.unsafeImmutable,
        factorWeightIdx.unsafeImmutable,
        factorFunction.unsafeImmutable,
        edgeIsPositiveF2V.unsafeImmutable,
        nonEvidenceVariables
      )
    }
  }

  def importDDFactorGraphReplicateHackOps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val CSRGraph = lookupTpe("CSRGraph")
    val DDFactorGraph = lookupTpe("DDFactorGraph")
    val DDFactorGraphReplicated = tpe("DDFactorGraphReplicated")

    val Control = grp("Control")

    data(DDFactorGraphReplicated,
      ("_copies", DenseVector(DDFactorGraph))
    )

    static (DDFactorGraphReplicated) ("apply", Nil, DenseVector(DDFactorGraph) :: DDFactorGraphReplicated) implements
      allocates(DDFactorGraphReplicated, ${$0})

    val DDFactorGraphReplicatedOps = withTpe(DDFactorGraphReplicated)
    DDFactorGraphReplicatedOps {
      compiler ("ddfgr_get_copies") (Nil :: DenseVector(DDFactorGraph)) implements getter(0, "_copies")

      infix ("local") (Nil :: DDFactorGraph) implements composite ${
        val socketId = 0
        ddfgr_get_copies($self).apply(socketId)
      }
    }

    direct (Control) ("replicate", Nil, DDFactorGraph :: DDFactorGraphReplicated) implements composite ${
      val numSockets = 1
      DDFactorGraphReplicated((0::numSockets).map({ isocket =>
        $0.deepcopy
      }))
    }
  }
}

