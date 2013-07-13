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

    direct (IO) ("readVector", Nil, ("path",MString) :: DenseVector(MDouble)) implements composite ${ readVector[Double]($path, v => v(0).toDouble) }

    direct (IO) ("readMatrix", Nil, ("path", MString) :: DenseMatrix(MDouble)) implements composite ${ readMatrix[Double]($path, s => s.toDouble) }
    direct (IO) ("readMatrix", Nil, (("path", MString), ("delim", MString)) :: DenseMatrix(MDouble)) implements composite ${ readMatrix[Double]($path, s => s.toDouble, $delim) }    

    val Elem = tpePar("Elem")

    // tab delimited by default
    direct (IO) ("readVector", Elem, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> Elem),("delim",MString,"\"\\s+\"")), DenseVector(Elem))) implements composite ${
      val a = ForgeFileReader.readLines($path){ line =>
        val tokens = line.trim.fsplit(delim)            
        val tokenVector = (0::array_length(tokens)) { i => tokens(i) }
        schemaBldr(tokenVector)
      }
      densevector_fromarray(a, true)
    }

    direct (IO) ("readMatrix", Elem, MethodSignature(List(("path",MString),("schemaBldr",MString ==> Elem),("delim",MString,"\"\\s+\"")), DenseMatrix(Elem))) implements composite ${    
      val a = ForgeFileReader.readLinesUnstructured($path){ (line:Rep[String], buf:Rep[ForgeArrayBuffer[Elem]]) =>
        val tokens = line.trim.fsplit(delim)
        for (i <- 0 until array_length(tokens)) {
          array_buffer_append(buf, schemaBldr(tokens(i)))
        }      
      }
      val numCols = array_length(readFirstLine(path).trim.fsplit(delim))
      densematrix_fromarray(a, array_length(a) / numCols, numCols)//.unsafeImmutable (needed?) 
    }

    compiler (IO) ("readFirstLine", Nil, ("path",MString) :: MString) implements codegen($cala, ${
      val xfs = new java.io.BufferedReader(new java.io.FileReader($path))
      val line = xfs.readLine()
      xfs.close()
      line      
    })
  }
}