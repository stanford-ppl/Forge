package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait TensorOps {
  this: OptiLADSL =>

  def importTensorOps() { 
    val A = tpePar("A")

    //importTensorNOps()

    for (r <- 3 until 6) {
      val DenseTensor = lookupTpe("DenseTensor" + r)
      val MArrayR = lookupTpe("Array" + r + "D")
      val name = DenseTensor.name.toLower

      data(DenseTensor, ("_data", MArrayR(A)))
      compiler (DenseTensor) (f"$name%s_raw_data", A, DenseTensor(A) :: MArrayR(A)) implements getter(0, "_data")
      compiler (DenseTensor) (f"$name%s_set_raw_data", A, (DenseTensor(A), MArrayR(A)) :: MUnit) implements setter(0, "_data". ${$1})
    
      compiler (DenseTensor) (f"$name%s_fromarray$r%dd", A, MArrayR(A) :: DenseTensor(A)) implements allocates(DenseTensor, ${$0})
    
      val TensorOps = withTpe(DenseTensor)
      TensorOps {
        // Element-wise multiplication
        infix ("*") (DenseTensor(A) :: DenseTensor(A), TArith(A)) implements composite ${
          $self.zip($1){(a,b) => implicitly[Arith[A]] * (a,b) }
        }
      }

      val wrapper: String => OpType = data => composite f"$name%s_fromarray$r%dd{$$data.as$r%dD}"
      compiler (DenseTensor) ("raw_data", A, DenseTensor(A) :: MArrayR(A)) implements composite f"densetensor$r%d_raw_data($$0)"
      addMultiArrayCommonOps(DenseTensor, r, "mul", wrapper)
      addTensorCommonOps(DenseTensor, r, wrapper)

      CastHelp.pairs{ (A, B, R) => 
        infix (DenseTensor) ("*", Nil, (DenseTensor(A), DenseTensor(B)) :: DenseTensor(R)) implements redirect { name + "_mul[" + R.name + "]" + CastHelp.casting(A,B)}
      }
    }
  }

  /*def importTensor3Ops() {
    val DenseTensor3 = lookupTpe("DenseTensor3")
    val A = tpePar("A")

    // --- Data fields
    data(DenseTensor3, ("_data", MArray3D(A)))
    compiler (DenseTensor3) ("densetensor3_raw_data", A, DenseTensor3(A) :: MArray3D(A)) implements getter(0, "_data")
    compiler (DenseTensor3) ("densetensor3_set_raw_data", A, (DenseTensor3(A), MArray3D(A)) :: MUnit) implements setter(0, "_data", ${$1})

    // --- Constructors
    compiler (DenseTensor3) ("densetensor3_fromarray3d", A, MArray3D(A) :: DenseTensor3(A)) implements allocates(DenseTensor3, ${$0})

    val wrapper3D: String => OpType = data => composite ${ densetensor3_fromarray3d{\$data.as3D} }
    compiler (DenseTensor3) ("raw_data", A, DenseTensor3(A) :: MArray3D(A)) implements composite ${ densetensor3_raw_data($0) }
    addMultiArrayCommonOps(DenseTensor3, 3, "mul", wrapper3D)
    addTensorCommonOps(DenseTensor3, 3, wrapper3D)
  }
  def importTensor4Ops() {
    val DenseTensor4 = lookupTpe("DenseTensor4")
    val A = tpePar("A")

    // --- Data fields
    data(DenseTensor4, ("_data", MArray4D(A)))
    compiler (DenseTensor4) ("densetensor4_raw_data", A, DenseTensor4(A) :: MArray4D(A)) implements getter(0, "_data")
    compiler (DenseTensor4) ("densetensor4_set_raw_data", A, (DenseTensor4(A), MArray4D(A)) :: MUnit) implements setter(0, "_data", ${$1})

    // --- Constructors
    compiler (DenseTensor4) ("densetensor4_fromarray4d", A, MArray4D(A) :: DenseTensor4(A)) implements allocates(DenseTensor4, ${$0})

    val wrapper4D: String => OpType = data => composite ${ densetensor4_fromarray4d{\$data.as4D} }
    compiler (DenseTensor3) ("raw_data", A, DenseTensor4(A) :: MArray4D(A)) implements composite ${ densetensor4_raw_data($0) }
    addMultiArrayCommonOps(DenseTensor4, 4, "mul", wrapper4D)
    addTensorCommonOps(DenseTensor4, 4, wrapper4D)
  }*/

  def addTensorCommonOps(MA: Rep[DSLType], ndims: Int, wrapper: Rep[String] => OpType) {
    val defaultReadDelims = List("\\s+/\\s+", "\\s+:\\s+", "\\s+;\\s+", "\\s+")
    val defaultWriteDelims = List("  /  ", "  :  ", "  ;  ", "    ")

    val IndexVector = lookupTpe("IndexVector")
    val A = tpePar("A")
    
    val name = MA.name
    val IndexVectorArgs = List.fill(ndims)(IndexVector)
    val MIntArgs = List.fill(ndims)(MInt)
    val IIntArgs = List.fill(ndims)(IInt)

    var argListN = Seq.tabulate(ndims){i => quotedArg(i)}.mkString(",")

    // --- Constructors
    static (MA) ("apply", A, MIntArgs :: MA(A), effect = mutable) implements wrapper ${ ArrayMD[A](\$argListN) } 
  
    // --- File Input
    // TODO: for fusion and cluster execution, reads should be pure. however, in that case we need a different way to order them with respect to writes / deletes.
    // one solution would be to implicitly convert strings to mutable file objects, and (manually) CSE future conversions to return the original mutable object.
    val rdelims = defaultReadDelims.drop(defaultReadDelims.length - ndims + 1).mkString(", ") 
    val wdelims = defaultWriteDelims.drop(defaultWriteDelims.length - ndims + 1).mkString(", ")
    val delimsArgs = Seq.tabulate(ndims-1){i => ("delim"+(i+1), MString) }
    val delims = Seq.tabulate(ndims-1){i => "$delim"+(i+1) }.mkString(",") 

    static (MA) ("fromFile", Nil, ("path", MString) :: MA(MDouble)) implements composite ${
      \$name.fromFile($path, \$rdelims, s => parse_string_todouble(s) )
    }
    static (MA) ("fromFile", Nil, ("path", MString) +: delimsArgs :: MA(MDouble)) implements composite ${ 
      \$name.fromFile($path, \$delims, s => parse_string_todouble(s) )
    }
    static (MA) ("fromFile", A, (("path", MString), ("bldr", MString ==> A)) :: MA(A)) implements composite ${
      \$name.fromFile($path, \$rdelims, $bldr)
    }
    static (MA) ("fromFile", A ("path", MString) +: delimsArgs :+ ("bldr", MString ==> A) :: MA(A)) implements wrapper ${
      fmultia_readfile($path, Seq("\\n", \$delims), $bldr)
    } 

    val TensorCommonOps = withTpe(MA)
    TensorCommonOps {
      argListN = Seq.tabulate(ndims){i => quotedArg(i+1)}.mkString(",")

      // --- File Output
      infix ("writeFile") (("path", MString) :: MUnit, TStringable(A), effect = simple) implements composite ${
        $self.writeFile($path, \$wdelims, e => e.makStr)
      }
      infix ("writefile") ( ("path", MString) +: delimsArgs :: MUnit, TStringable(A), effect = simple) implements composite ${
        $self.writeFile($path, \$delims, e => e.makeStr)
      }
      infix ("writeFile") ( (("path", MString), ("bldr", A ==> MString)), effect = simple) implements composite ${
        $self.writeFile($path, \$delims, $bldr) 
      }
      infix ("writeFile") ( ("path", MString) +: delimsArgs :+ ("bldr", A ==> MString), effect = simple) implements composite ${
        fmultia_writefile(raw_data($self), Seq(\$delims), $path, $bldr)
      }

      // --- Single element access
      infix ("apply") (MIntArgs :: A) implements composite ${ raw_data($self).apply(\$argListN) }

      // --- Continuous slices
      infix ("slice") (IndexVectorArgs :: MA(A)) implements wrapper ${
        val indvs = Seq(\$argListN)
        fassert(indvs.map{isRange(_)}.reduce{_&&_}, "\$name slice is not defined for discontinuous IndexVectors")
        val ofs  = indvs.map{iv => if (isWild(iv)) unit(0) else iv.start}
        val str  = Seq.fill(\$ndims)(unit(0))
        val dims = indvs.zipWithIndex.map{i => if (isWild(i._1)) $self.dim(i._2) else i._1.length}
        fmultia_view(raw_data($self), ofs, str, dims)
      }

      infix ("apply") (IndexVectorArgs :: MA(A)) implements wrapper ${
        val indvs = Seq(\$argListN)
        if (indvs.map{isRange(_)}.reduce{_&&_}) $self.slice(\$argListN)
        else $self.dice(\$argListN)
      }

      // --- Discontinuous dices
      var loopIndices = Seq.tabulate(ndims){i => quotedArg(i+1) + "(li(" + i + "))"}.mkString(",")
      infix ("dice") (IndexVectorArgs :: MA(A)) implements wrapper ${
        val indvs = Seq(\$argListN)
        if (indvs.map{isWild(_)}.reduce{_&&_}) raw_data($self).Clone
        else if (indvs.map{isRange(_)}.reduce{_&&_}) raw_data($self.slice(\$argListN)).Clone
        else {
          val inds = indvs.zipWithIndex.map{i => if (isWild(i._1)) $self.dimIndices(i._2) else i._1 }
          val dims = inds.map{_.length}
          fmultia_fromfunction(dims){li => $self(\$loopIndices) }
        }
      }

      // --- Updates
      val argRHS = quotedArg(ndims+1)
      infix ("update") (MIntArgs :+ A :: MUnit, effect = write(0)) implements composite ${
        raw_data($self).update(\$argListN, \$argRHS)
      }

      infix ("update") (IndexVectorArgs :+ A :: MUnit, effect = write(0)) implements composite ${
        val indvs = Seq(\$argListN)
        if (indvs.map{isWild(_)}.reduce{_&&_}) {
          $self.mmap{e => \$argRHS}  // Updates all elements
        }
        else {  // Can't use slice.mmap even if all are Ranges - slices are immutable

        }
      }

      val inds = Seq.tabulate(ndims){d => ('i' + d).toChar.toString}.mkString(",")
      val scatterInds = Seq.tabulate(ndims){ d => quotedArg(d) + "(" + ('i' + d).toChar.toString + ")"}.mkString(",")
      infix ("update") (IndexVectorArgs :+ MA(A) :: MUnit, effect = write(0)) implements composite ${
        val indvs = Seq(\$argListN)
        if (indvs.map{isWild(_)}.reduce{_&&_}) {  // Full copy
          fassert(!fmultia_shape_mismatch($self, \$argRHS), "Dimension mismatch in \$name copy")
          $self.mzip(\$argRHS){(a,b) => b}
        }
        else {
          // Scatter
          \$argRHS.forIndices{(\$inds) => $self(\$scatterInds) = \$argRHS(\$inds) }
        }
      }

      // --- Permuting
      if (ndims > 2) {
        infix ("permute") (IIntArgs :: MA(A)) implements wrapper ${
          fmultia_permute(raw_data($self), Seq(\$argListN))
        }
        infix ("vpermute") (IIntArgs :: MA(A)) implements wrapper ${
          fmultia_permute_view(raw_data($self), Seq(\$argListN))
        }
      }
      
      /*val a = quotedArg(1)
      val b = quotedArg(2)
      if (ndims >= 1) {
        val wilds = if (ndims == 1) "" else "," + Seq.fill(ndims - 1)("*").mkString(",")
        infix ("diceRows") (IndexVector :: MA(A)) implements composite s"""self.dice($a$wilds)""" //"self.dice(" + quotedArg(1) + "," + wilds + ")"
        //TODO: infix ("sliceRow") (MInt :: ???) implements composite s"""self.slice(idx($a)$wilds)"""
        infix ("sliceRows") (IndexVector :: MA(A)) implements composite s"""self.slice($a$wilds)""" //self.slice(" + quotedArg(1) + "," + wilds + ")"
        infix ("sliceRows") ((MInt, MInt) :: MA(A)) implements composite s"""self.slice($a::$b$wilds)""" //self.slice(" + quotedArg(1) + "::" + quotedArg(2) + "," + wilds + ")"
        infix ("updateRow") ((MInt, A) :: MUnit, effect = write(0)) implements composite s"""self(idx($a)$wilds) = $b"""
        //infix ("updateRow") ((MInt, ???) :: MUnit, effect = write(0)) implements composite s"""self(idx($a)$wilds) = $b"""
        infix ("updateRows") ((IndexVector, A) :: MUnit, effect = write(0)) implements composite s"""self($a$wilds) = $b"""
        infix ("updateRows") ((IndexVector, MA(A)) :: MUnit, effect = write(0)) implements composite s"""self($a$wilds) = $b"""
      }
      if (ndims >= 2) {
        val wilds = if (ndims == 2) "" else "," + Seq.fill(ndims - 2)("*").mkString(",")
        infix ("diceCols") (IndexVector :: MA(A)) implements composite s"""self.dice(*,$a$wilds)""" //"self.dice(*," + quotedArg(1) + "," + wilds + ")"
        //TODO: infix ("sliceCol") (MInt :: ???) implements composite "self.slice(*,idx(" + quotedArg(1) + ")," + wilds + ")"
        infix ("sliceCols") (IndexVector :: MA(A)) implements composite s"""self.slice(*,$a$wilds)""" //self.slice(*," + quotedArg(1) + "," + wilds + ")"
        infix ("sliceCols") ((MInt, MInt) :: MA(A)) implements composite s"""self.slice(*,$a::$b$wilds)""" //"self.slice(*," + quotedArg(1) + "::" + quotedArg(2) + "," + wilds + ")"
        infix ("updateCol") ((MInt, A) :: MUnit, effect = write(0)) implements composite s"""self(*,idx($a)$wilds) = $b"""
        //infix ("updateCol") ((MInt, ???) :: MUnit, effect = write(0)) implements composite s"""self(*,idx($a)$wilds) = $b"""
        infix ("updateCols") ((IndexVector, A) :: MUnit, effect = write(0)) implements composite s"""self(*,$a$wilds) = $b"""
        infix ("updateCols") ((IndexVector, MA(A)) :: MUnit, effect = write(0)) implements composite s"""self(*,$a$wilds) = $b"""
      }
      if (ndims >= 3) {
        val wilds = if (ndims == 3) "" else "," + Seq.fill(ndims - 3)("*").mkString(",")
        infix ("dicePages") (IndexVector :: MA(A)) implements composite s"""self.dice(*,*,$a$wilds)""" //"self.dice(*,*," + quotedArg(1) + "," + wilds + ")"
        //TODO: infix ("slicePage") (MInt :: ???) implements composite "self.slice(*,*,idx(" + quotedArg(1) + ")," + wilds + ")"
        infix ("slicePages") (IndexVector :: MA(A)) implements composite s"""self.slice(*,*,$a$wilds)""" //"self.slice(*,*," + quotedArg(1) + "," + wilds + ")"
        infix ("slicePages") ((MInt, MInt) :: MA(A)) implements composite s"""self.slice(*,*,$a::$b$wilds)""" //"self.slice(*,*," + quotedArg(1) + "::" + quotedArg(2) + "," + wilds + ")"
        infix ("updatePage") ((MInt, A) :: MUnit, effect = write(0)) implements composite s"""self(*,*,idx($a)$wilds) = $b"""
        //infix ("updatePage") ((MInt, ???) :: MUnit, effect = write(0)) implements composite s"""self(*,*,idx($a)$wilds) = $b"""
        infix ("updatePages") ((IndexVector, A) :: MUnit, effect = write(0)) implements composite s"""self(*,*,$a$wilds) = $b"""
        infix ("updatePages") ((IndexVector, MA(A)) :: MUnit, effect = write(0)) implements composite s"""self(*,*,$a$wilds) = $b"""
      }
      if (ndims >= 4) {
        val wilds = if (ndims == 4) "" else "," + Seq.fill(ndims - 4)("*").mkString(",")
        infix ("diceBanks") (IndexVector :: MA(A)) implements composite s"""self.dice(*,*,*,$a$wilds)""" //+ quotedArg(1) + "," + wilds + ")"
        //TODO: infix ("sliceBank") (MInt :: ???) implements composite "self.slice(*,*,*,idx(" + quotedArg(1) + ")," + wilds + ")"
        infix ("sliceBanks") (IndexVector :: MA(A)) implements composite s"""self.slice(*,*,*,$a$wilds)"""
        infix ("sliceBanks") ((MInt, MInt) :: MA(A)) implements composite s"""self.slice(*,*,*,$a::$b$wilds)""" //+ quotedArg(1) + "::" + quotedArg(2) + "," + wilds + ")"
        infix ("updateBank") ((MInt, A) :: MUnit, effect = write(0)) implements composite s"""self(*,*,*,$a$wilds) = $b"""
        //infix ("updateBank") ((MInt, ???) :: MUnit, effect = write(0)) implements composite s"""self(*,*,*,$a$wilds) = $b"""
        infix ("updateBanks") ((IndexVector, A) :: MUnit, effect = write(0)) implements composite s"""self(*,*,*,$a$wilds) = $b""" 
        infix ("updateBanks") ((IndexVector, MA(A)) :: MUnit, effect = write(0)) implements composite s"""self(*,*,*,$a$wilds) = $b"""
      }*/
    }
  }
}
