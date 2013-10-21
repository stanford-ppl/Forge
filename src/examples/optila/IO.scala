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

    // -- input

    compiler (IO) ("optila_todouble", Nil, MString :: MDouble) implements single ${
      if ($0 == "Inf") INF
      else if ($0 == "-Inf") nINF
      else $0.toDouble
    }

    // TODO: for fusion and cluster execution, reads should be pure. however, in that case we need a different way to order them with respect to writes / deletes.
    // one solution would be to implicitly convert strings to mutable file objects, and (manually) CSE future conversions to return the original mutable object.

    direct (IO) ("readVector", Nil, ("path",MString) :: DenseVector(MDouble)) implements composite ${ readVector[Double]($path, v => optila_todouble(v(0))) }

    direct (IO) ("readMatrix", Nil, ("path", MString) :: DenseMatrix(MDouble)) implements composite ${ readMatrix[Double]($path, s => optila_todouble(s)) }
    direct (IO) ("readMatrix", Nil, (("path", MString), ("delim", MString)) :: DenseMatrix(MDouble)) implements composite ${ readMatrix[Double]($path, s => optila_todouble(s), $delim) }

    val Elem = tpePar("Elem")

    // whitespace delimited by default
    direct (IO) ("readVector", Elem, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> Elem),("delim",MString,"\"\\s+\"")), DenseVector(Elem)), effect = simple) implements single ${
      val a = ForgeFileReader.readLines($path){ line =>
        val tokens = line.trim.fsplit(delim)
        val tokenVector = (0::array_length(tokens)) { i => tokens(i) }
        schemaBldr(tokenVector)
      }
      densevector_fromarray(a, true)
    }

    direct (IO) ("readMatrix", Elem, MethodSignature(List(("path",MString),("schemaBldr",MString ==> Elem),("delim",MString,"\"\\s+\"")), DenseMatrix(Elem)), effect = simple) implements single ${
      val a = ForgeFileReader.readLinesUnstructured($path){ (line:Rep[String], buf:Rep[ForgeArrayBuffer[Elem]]) =>
        val tokens = line.trim.fsplit(delim)
        for (i <- 0 until array_length(tokens)) {
          array_buffer_append(buf, schemaBldr(tokens(i)))
        }
      }
      val numCols = array_length(readFirstLine(path).trim.fsplit(delim))
      densematrix_fromarray(a, array_length(a) / numCols, numCols).unsafeImmutable // unsafeImmutable needed due to struct unwrapping Reflect(Reflect(..)) bug (see LAInputReaderOps.scala line 46 in Delite)
    }

    compiler (IO) ("readFirstLine", Nil, ("path",MString) :: MString) implements codegen($cala, ${
      val xfs = new java.io.BufferedReader(new java.io.FileReader($path))
      val line = xfs.readLine()
      xfs.close()
      line
    })


    // -- output

    direct (IO) ("writeVector", Elem withBound TStringable, (("v",DenseVector(Elem)),("path",MString)) :: MUnit, effect = simple) implements composite ${
      write_vector_helper($path, densevector_raw_data($v.map(_.makeStr)), $v.length)
    }

    compiler (IO) ("write_vector_helper", Nil, (("path",MString),("data",MArray(MString)),("length",MInt)) :: MUnit, effect = simple) implements codegen($cala, ${
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter($path))
      for (i <- 0 until $length) {
        xfs.write($data(i) + "\\n")
      }
      xfs.close()
    })

    direct (IO) ("writeMatrix", Elem withBound TStringable, MethodSignature(List(("m",DenseMatrix(Elem)),("path",MString),("delim",MString,"\"    \"")), MUnit), effect = simple) implements composite ${
      write_matrix_helper($path, densematrix_raw_data($m.map(_.makeStr)), $m.numRows, $m.numCols, $delim)
    }

    compiler (IO) ("write_matrix_helper", Nil, (("path",MString),("data",MArray(MString)),("numRows",MInt),("numCols",MInt),("delim",MString)) :: MUnit, effect = simple) implements codegen($cala, ${
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter($path))
      for (i <- 0 until $numRows) {
        for (j <- 0 until $numCols) {
          xfs.write($data(i*$numCols+j) + $delim)
        }
        xfs.write("\\n")
      }
      xfs.close()
    })


    // -- utility

    direct (IO) ("deleteFile", Nil, MString :: MUnit, effect = simple) implements codegen($cala, ${
      val f = new java.io.File($0)
      if (f.exists) f.delete()
      ()
    })

  }
}
