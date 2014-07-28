package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}
import factor._

trait MLIOOps {
  this: OptiMLDSL =>

  lazy val IO = grp("MLio")

  def importMLIOOps() {
    importFactorIOOps()
    importARFFOps()
  }

  def importFactorIOOps() {  
    val T = tpePar("T")
    val DenseVector = lookupTpe("DenseVector")
    val FactorGraph = lookupTpe("FactorGraph")
    val Weight = lookupTpe("Weight")
    val FactorVariable = lookupTpe("FactorVariable")
    val Variable = lookupTpe("RandomVariable")
    val FunctionFactor = lookupTpe("FunctionFactor")
    val Tup3 = lookupTpe("Tup3")
    val Tup4 = lookupTpe("Tup4")

    // -- temporary: use java.io.DataInputStream to read binary format, until Delite supports a fixed-length binary reader

    val DataInputStream = tpe("java.io.DataInputStream")
    compiler (IO) ("datainputstream_new", Nil, ("path",MString) :: DataInputStream, effect = simple) implements codegen ($cala, ${
      new java.io.DataInputStream(new java.io.FileInputStream($path))
    })

    infix (IO) ("available", Nil, DataInputStream :: MInt, effect = simple) implements codegen ($cala, ${
      $0.available()
    })

    // infix close clashes with LMS IOOps
    infix (IO) ("fclose", Nil, DataInputStream :: MUnit, effect = simple) implements codegen ($cala, ${
      $0.close()
    })

    infix (IO) ("readShort", Nil, DataInputStream :: MShort, effect = simple) implements codegen ($cala, ${
      $0.readShort()
    })

    infix (IO) ("readInt", Nil, DataInputStream :: MInt, effect = simple) implements codegen ($cala, ${
      $0.readInt()
    })

    infix (IO) ("readLong", Nil, DataInputStream :: MLong, effect = simple) implements codegen ($cala, ${
      $0.readLong()
    })

    infix (IO) ("readDouble", Nil, DataInputStream :: MDouble, effect = simple) implements codegen ($cala, ${
      $0.readDouble()
    })

    infix (IO) ("readBoolean", Nil, DataInputStream :: MBoolean, effect = simple) implements codegen ($cala, ${
      $0.readBoolean()
    })

    // -- input

    compiler (IO) ("fg_read_weights", Nil, ("path",MString) :: DenseVector(Weight)) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[Weight](0, true)
      while (dis.available() > 0) {
        val weightId = dis.readLong().toInt
        val isFixed = dis.readBoolean()
        val initialValue = dis.readDouble()   

        out <<= Weight(weightId, initialValue, isFixed)
      }
      dis.fclose()
      out.unsafeImmutable
    }

    compiler (IO) ("fg_read_variables", Nil, ("path",MString) :: DenseVector(Variable)) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[RandomVariable](0, true)
      while (dis.available() > 0) {
        val variableId = dis.readLong().toInt
        val isEvidence = dis.readBoolean()
        val initialValue = dis.readDouble()
        val dataType = dis.readShort()
        val edgeCount = dis.readLong().toInt
        val cardinality = dis.readLong().toInt
        val isQuery = !isEvidence
        
        out <<= RandomVariable(variableId, DenseVector(0.0, 1.0), initialValue, isEvidence, isQuery)
      }
      dis.fclose()
      out.unsafeImmutable
    }

    compiler (IO) ("fg_read_edges", Nil, ("path",MString) :: DenseVector(Tup4(MInt,MInt,MBoolean,MInt))) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[Tup4[Int,Int,Boolean,Int]](0, true)
      while (dis.available() > 0) {
        val variableId = dis.readLong().toInt
        val factorId = dis.readLong().toInt
        val position = dis.readLong().toInt
        val isPositive = dis.readBoolean()
        val equalPredicate = dis.readLong().toInt
        
        out <<= pack((variableId, factorId, isPositive, position))
      }
      dis.fclose()
      out.unsafeImmutable
    }

    compiler (IO) ("fg_read_factors", Nil, ("path",MString) :: DenseVector(Tup3(MInt,MInt,MInt))) implements single ${
      val dis = datainputstream_new($path)
      val out = DenseVector[Tup3[Int,Int,Int]](0, true)
      while (dis.available() > 0) {
        val factorId = dis.readLong().toInt
        val weightId = dis.readLong().toInt
        val factorFunction = dis.readShort().AsInstanceOf[Int]
        val edgeCount = dis.readLong().toInt
        out <<= pack((factorId, weightId, factorFunction))
      }
      dis.fclose()
      out.unsafeImmutable
    }

    direct (IO) ("readFactorGraph", Nil, MethodSignature(List(("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("edgesPath", MString), ("delim",MString,"unit(\"\\t\")")), FactorGraph(FunctionFactor))) implements composite ${
      val weights = fg_read_weights($weightsPath).sortBy(w => w.id)      
      val variables = fg_read_variables($variablesPath).distinct.sortBy(r => r.id)      
      val edges = fg_read_edges($edgesPath)
      
      val factorVariablesMap = edges.groupBy(r => r._2, r => FactorVariable(r._1, r._3, DenseVector(0.0, 1.0), r._4))      
      val factorRows = fg_read_factors($factorsPath)      
      val allFactors = factorRows.map { t =>
        val vars = if (factorVariablesMap.contains(t._1)) factorVariablesMap(t._1).sortBy(r => r.position) else DenseVector[FactorVariable]()
        FunctionFactor(t._1, vars, t._2, t._3)
      }
      val factors = allFactors.filter(f => f.vars.length > 0).sortBy(f => f.id)

      val variablesToFactors = build_variable_factors(variables, factors)
      val variableValues = variables.map(v => v.value).mutable
      val weightValues = weights.map(w => w.value).mutable

      FactorGraph(factors, variables, weights, variablesToFactors, variableValues, weightValues)      
    }

    /*
    // old DeepDive factor graph formats
    direct (IO) ("readFactorGraph", Nil, MethodSignature(List(("factorsPath", MString), ("variablesPath", MString), ("weightsPath", MString), ("delim",MString,"\"\\t\"")), FactorGraph(FunctionFactor))) implements composite ${
      val weights = densevector_fromarray(ForgeFileReader.readLines($weightsPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (id, initialValue, isFixed) = (tokens(0).toInt, tokens(1).toDouble, tokens(2).toBoolean)
        Weight(id, initialValue, isFixed)
      }, true).sortBy(w => w.id)
      
      val variableRows = ForgeFileReader.readLines($variablesPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (variableId, factorId, position, isPositive, dataType, initialValue, isEvidence, isQuery) = (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt, tokens(3).toBoolean, tokens(4), tokens(5).toDouble, tokens(6).toBoolean, tokens(7).toBoolean)
        pack((variableId, factorId, position, isPositive, dataType, initialValue, isEvidence, isQuery))
      }

      // currently only supporting boolean vars
      val variables = densevector_fromarray(variableRows, true).map(r => RandomVariable(r._1, DenseVector(0.0, 1.0), r._6, r._7, r._8)).distinct.sortBy(r => r.id)
      
      val factorVariablesMap = densevector_fromarray(variableRows, true).groupBy(r => r._2, r => FactorVariable(r._1, r._4, DenseVector(0.0, 1.0), r._3))

      // reading factors breaks in parallel if we try to look up the hashmap inside the file reading loop, for some reason
      val factorRows = ForgeFileReader.readLines($factorsPath) { line =>
        val tokens = line.trim.fsplit(delim)
        val (factorId, weightId, func) = (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt)
        // val vars = if (factorVariablesMap.contains(factorId)) factorVariablesMap(factorId) else DenseVector[FactorVariable]()
        // FunctionFactor(factorId, vars, weightId, func)
        pack((factorId, weightId, func))
      }
      val allFactors = factorRows.map { t =>
        val vars = if (factorVariablesMap.contains(t._1)) factorVariablesMap(t._1).sortBy(r => r.position) else DenseVector[FactorVariable]()
        FunctionFactor(t._1, vars, t._2, t._3)
      }
      val factors = densevector_fromarray(allFactors, true).filter(f => f.vars.length > 0).sortBy(f => f.id)
      
      val variablesToFactors = build_variable_factors(variables, factors)
      val variableValues = variables.map(v => v.value).mutable
      val weightValues = weights.map(w => w.value).mutable

      FactorGraph(factors, variables, weights, variablesToFactors, variableValues, weightValues)      
    }
    */

    // -- output



    // -- utility

    /* builds reverse mapping from variables -> factors */
    compiler (IO) ("build_variable_factors", Nil, (("variables", DenseVector(Variable)), ("factors", DenseVector(FunctionFactor))) :: DenseVector(DenseVector(MInt))) implements composite ${
      val variablesToFactors = DenseVector[DenseVector[Int]](variables.length, true) 

      for (i <- 0 until variablesToFactors.length) {
        variablesToFactors(i) = DenseVector[Int]()
      }

      val factorIds = factors.indices
      for (i <- 0 until factorIds.length) {
        val f = factors(factorIds(i))
        for (j <- 0 until f.vars.length) {
          val vId = f.vars.apply(j).id
          val curVec = variablesToFactors(vId)
          variablesToFactors.update(vId, curVec << factorIds(i))
        }
      }
   
      variablesToFactors.map(v => v.distinct)      
    }

    ()
  
  }

  def importARFFOps() {
  	val Row = tpePar("Row")
  	val DenseVector = lookupTpe("DenseVector")

  	direct (IO) ("readARFF", Row, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> Row)), DenseVector(Row)), effect = simple) implements composite ${
  	  // INVESTIGATE: lines and start computations, and body and map computations, fuse unsafely without .mutable
  	  // the latter appears to be the bug of not having the filter condition properly guard the subsequent computation
  	  val lines = densevector_fromarray(ForgeFileReader.readLines($path){ line => line.trim }, true).mutable
  	  
  	  // skip past the header to the data section
      // since we are using schemaBldr, we don't care about the attribute types
  	  val start = lines find { _ == "@DATA" }
  	  if (start.length < 1) fatal("could not find @DATA tag in ARFF file: " + $path)
  	  val body = lines.drop(start(0)+1).filter(!_.startsWith("%")).mutable
  	  body map { s => schemaBldr(densevector_fromarray(s.fsplit(","), true)) }
    }
  }
}
