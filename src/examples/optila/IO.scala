package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait IOOps {
  this: OptiLADSL => 
 
  def importIOOps() {
    val IO = grp("LAio") // avoid conflict with IOOps in LMS 
    
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    // only the simple versions for now    
    direct (IO) ("readVector", Nil, ("path",MString) :: DenseVector(MDouble)) implements single ${
      val a = ForgeFileReader.readLines($path)(s => s.trim.toDouble)
      val out = DenseVector[Double](array_length(a), true)
      densevector_set_raw_data(out, a)
      out.unsafeImmutable
    }
    
    // tab delimited by default 
    direct (IO) ("readMatrix", Nil, MethodSignature(List(("path",MString), ("delim",MString,"\"\\s+\"")), DenseMatrix(MDouble))) implements single ${
      val a = ForgeFileReader.readLines($path)(s => s)
      val s0 = a(0).trim.fsplit($delim)
      val out = DenseMatrix[Double](array_length(a),array_length(s0))
      for (i <- 0 until array_length(a)) {        
        val s = a(i).trim.fsplit($delim)
        for (j <- 0 until array_length(s)) {
          out(i,j) = s(j).toDouble
        }
      }
      out.unsafeImmutable
    }    
  }
}