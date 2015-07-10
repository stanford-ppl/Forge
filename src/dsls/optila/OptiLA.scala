package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication, ForgeApplicationRunner}

object OptiLADSLRunner extends ForgeApplicationRunner with OptiLADSL

trait OptiLADSL extends ForgeApplication 
		with MultiArrayOps 			with VectorOps 				with DenseVectorOps
		with IndexVectorOps 		with DenseMatrixOps   with TensorOps
    with MultiArrayConstructors
    with SparseMatrixOps    with SparseVectorOps  with SparseVectorViewOps

	  with ArithOps  					with BasicMathOps  		with OrderOps 
	  with StringableOps      with RandomOps        with ShapeOps
	  with ComplexOps 				with LinAlgOps 				with LegacySupport 
  {

  override def dslName = "OptiLA2"
  override def addREPLOverride = true

  def specification() = {
  	// Scala Ops (Numeric and Fractional are replaced by Arith)
    importPrimitives()
    importMisc()
    importCasts()
    importOrdering()
    importStrings()
    importMath()
    importTuples()
    importHashMap()
    importConcurrentHashMap()

    // Declare types first so that they are available to all ops
    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")

    // Delite/Forge internals
    // TODO: These should eventually be added to Forge as a whole.
    extern(grp("MultiArraySupport"))	
    val MArray1D = tpe("Array1D", T)
    val MArray2D = tpe("Array2D", T)
    val MArray3D = tpe("Array3D", T)
    val MArray4D = tpe("Array4D", T)
    val MArray5D = tpe("Array5D", T)
    val MArrayMD = tpe("ArrayMD", T)
    val MMap = tpe("ForgeMap", List(K, V))
    primitiveTypes :::= List(MArray1D, MArray2D, MArray3D, MArray4D, MArray5D, MArrayMD, MMap)

    // OptiLA Structs
    val IndexVector = tpe("IndexVector")
    val DenseVector = tpe("DenseVector", T)
    val DenseMatrix = tpe("DenseMatrix", T)
    val DenseTensor3 = tpe("DenseTensor3", T) // These need better names..
    val DenseTensor4 = tpe("DenseTensor4", T)
    val DenseTensor5 = tpe("DenseTensor5", T)
    val DenseTensorN = tpe("DenseTensorN", T)
    val SparseVector = tpe("SparseVector", T)
    val SparseVectorView = tpe("SparseVectorView", T)
    val SparseMatrix = tpe("SparseMatrix", T)
    val SparseMatrixBuildable = tpe("SparseMatrixBuildable", T)
    val Ctr = grp("Constructors")
    val IO = grp("LAio")    // avoid conflict with IOOps in LMS

    importCompilerSeqLoops()

    importArithOps()
    importOrderOps()
    importBasicMathOps()
    importRandomOps()
    importStringableOps()
    importComplexOps()  

    importNumericStringFormatting()

    importIndexVectorOps()
    importDenseVectorOps()
    importDenseMatrixOps()
    importTensorOps()
    importSparseVectorOps()
    importSparseVectorViewOps()
    importSparseMatrixOps()
    importConstructors()
    importLinAlgOps()
    importShapeOps()
    importLegacySupport()

    // native libs
    extern(grp("BLAS"))
    extern(grp("LAPACK"))

    // rewrites
    extern(grp("Rewrite"), targets = Nil)
  }

  // Sequential loops
  def importCompilerSeqLoops() {
    val Range = tpe("Range")
    data(Range, ("start", MInt), ("end", MInt))
    compiler (Range) ("range_start", Nil, Range :: MInt) implements getter(0, "start")
    compiler (Range) ("range_end", Nil, Range :: MInt) implements getter(0, "end")

    noInfixList :::= List("infix_foreach")
    compiler (Range) ("infix_until", Nil, (MInt,MInt) :: Range) implements allocates(Range, quotedArg(0), quotedArg(1))

    // infix_foreach must be compiler only both so that it is not used improperly and to not interfere with other codegen nodes in the library
    // this is a little convoluted unfortunately (because of the restriction on passing structs to codegen nodes)
    compiler (Range) ("infix_foreach", Nil, (Range, MInt ==> MUnit) :: MUnit) implements composite ${ range_foreach(range_start($0), range_end($0), $1) }
    val range_foreach = compiler (Range) ("range_foreach", Nil, (("start",MInt),("end",MInt),("func",MInt ==> MUnit)) :: MUnit) 
    impl (range_foreach) (codegen($cala, ${
      var i = $start
      while (i < $end) {
        $b[func](i)
        i += 1
      }
    }))

    impl (range_foreach) (codegen(cpp, ${
      for(int i=$start ; i<$end ; i++) {
        $b[func](i)
      }
    }))
  }

  def importNumericStringFormatting() {
    val FString = lookupGrp("FString")
    val T = tpePar("T")
    // override default string formatting (numericPrecision is a global defined in extern)
    // we use "" + $a instead of $a.toString to avoid an NPE when explicitly calling toString inside the REPL
    val formatStr = {
      val a = quotedArg(0)
      val f = "(\"% .\"+Global.numericPrecision+\"g\")" // can't escape quotes inside string interpolation scope

      s"""
      def numericStr[T](x: T) = {
        val s = $f.format(x)
        val padPrefix = (Global.numericPrecision+6) - s.length
        if (padPrefix > 0) " "*padPrefix + s else s
      }
      if ($a.isInstanceOf[Double] || $a.isInstanceOf[Float]) 
        numericStr($a) 
      else 
        ("" + $a)
      """
    }

    val fmt_str = direct (FString) ("optila_fmt_str", T, T :: MString) 
    impl (fmt_str) (codegen($cala, formatStr))
    impl (fmt_str) (codegen(cpp, "convert_to_string<" +  unquotes("remapWithRef("+opArgPrefix+"0.tp)") + " >(" + quotedArg(0) + ")"))

    compiler (FString) ("optila_padspace", Nil, MString :: MString) implements composite ${
      "  " + $0
      // if ($0.startsWith("-")) "  " + $0 else "   " + $0
    }

    compiler (FString) ("parse_string_todouble", Nil, MString :: MDouble) implements single ${
      if ($0 == "Inf") INF
      else if ($0 == "-Inf") nINF
      else $0.toDouble
    }

    /*val formatFixed = { 
      val a = quotedArg(0)
      
      s"""
      var s = "" + (( $a & 0xFFFF0000) >> 16) + "."
      var r = $a & 0x0000FFFF
      var i = 0
      while (i < Global.numericPrecision && r != 0) {
        r *= 10
        s += "" + (r >> 16)
        r = (r & 0x0000FFFF)
        i += 1
      }
      s
      """
    }

    val fmt_fixed = direct (lookupGrp("FString")) ("optila_fmt_fixed", Nil, MInt :: MString)
    impl (fmt_fixed) (codegen($cala, formatFixed))*/
  }

  // This helps to reduce the amount of DSL spec code required with implicit type casting when 
  // doing operations on differently typed matrices/vectors/multiarrays - it doesn't help with
  // the number of generated function combinations, however. (actually is more with MLong added)
  object CastHelp {
    // If Fixed Point is ever a separate type, it can be added (below MFloat?)
    // Note exponential increase in generated code though
    val consts = List(CInt, CLong, CFloat, CDouble)
    val prims = List(MInt, MLong, MFloat, MDouble)
  	val casts = List(".toInt", ".toLong", ".toFloat", ".toDouble")

  	// For all primitive pairs (e.g. (MInt, MDouble) => MDouble, etc.)
  	def pairs(f: (Rep[DSLType], Rep[DSLType], Rep[DSLType]) => Any) { 
  		prims.zipWithIndex.foreach { l =>
  			prims.zipWithIndex.foreach { r =>
  				f(l._1, r._1, prims(l._2 max r._2))
  		}}
  	}
  	// All left dominant primitive pairs (e.g. (MDouble, MInt) => MDouble, etc.)
  	def leftDomPairs(f: (Rep[DSLType], Rep[DSLType]) => Any) { 
  		prims.zipWithIndex.foreach{ l =>
  			prims.zipWithIndex.foreach { r => 
  				if (l._2 >= r._2) f(l._1, r._1)
  		}}
  	}

    // TODO: Implicit lifting should make these three unnecessary, but for some reason
    // that isn't always happening with infix operations right now. These are added as a hack
  	// All const-prim pairs (e.g. (CInt, MDouble) => MDouble, etc.)
  	def leftConsts(f: (Rep[DSLType], Rep[DSLType], Rep[DSLType]) => Any) {
  		consts.zipWithIndex.foreach {l =>
  			prims.zipWithIndex.foreach {r =>
  				f(l._1, r._1, prims(l._2 max r._2))
  		}}
  	}
  	// All prim-const pairs (e.g. (CDouble, MInt) => MDouble, etc.)
  	def rightConsts(f: (Rep[DSLType], Rep[DSLType], Rep[DSLType]) => Any) {
  		prims.zipWithIndex.foreach {l =>
  			consts.zipWithIndex.foreach {r =>
  				f(l._1, r._1, prims(l._2 max r._2))
  		}}
  	}
  	// All left dominant prim-const pairs (e.g. (MDouble, CInt) => MDouble, etc.)
  	def leftDomRightConsts(f: (Rep[DSLType], Rep[DSLType]) => Any) {
  		prims.zipWithIndex.foreach {l =>
  			consts.zipWithIndex.foreach {r =>
  				if (l._2 >= r._2) f(l._1, r._1)
  		}}
  	}

  	def casting(a: Rep[DSLType]*): String = {
  		val top = a.map{e => prims.indexOf(e) max consts.indexOf(e)}.max
  		val cast = casts(top)
  		a.zipWithIndex.map{e => 
  			val index = if (prims.contains(e._1)) prims.indexOf(e._1) else consts.indexOf(e._1)
  			val carg = quotedArg(e._2) + (if (index == top) "" else cast) 
  			if (prims.indexOf(e._1) >= 0) carg else "unit(" + carg + ")"
  		}.mkString("(", ",", ")")
  	}

    // used when the arguments need to be scrambled before use in whatever function they're
    // being redirected to, e.g. Scalar + Matrix -> matrix_add(cast($1), cast($0))
  	def reorderCasting(a: Tuple2[Int, Rep[DSLType]]*): String = {
  		val args = a.map{e => e._1}
  		val dsltpes = a.map{e => e._2}
  		val top = dsltpes.map{e => prims.indexOf(e) max consts.indexOf(e)}.max
  		val cast = casts(top)
  		dsltpes.zipWithIndex.map{e => 
  			val index = if (prims.contains(e._1)) prims.indexOf(e._1) else consts.indexOf(e._1)
  			val carg = quotedArg(args(e._2)) + (if (index == top) "" else cast) 
  			if (prims.indexOf(e._1) >= 0) carg else "unit(" + carg + ")"
  		}.mkString("(", ",", ")")
  	}
  }
}
