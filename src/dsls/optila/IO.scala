package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait IOOps {
  this: OptiLADSL =>

  def importIOOps() {
    val IO = grp("LAio") // avoid conflict with IOOps in LMS

    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    // -- input

    compiler (IO) ("optila_todouble", Nil, MString :: MDouble) implements composite ${
      if ($0 == "Inf") INF
      else if ($0 == "-Inf") nINF
      else $0.toDouble
    }

    /**
     * For fusion and cluster execution, reads should be pure. however, in that case we need a different way to order them with respect to writes / deletes.
     * one solution would be to implicitly convert strings to mutable file objects, and (manually) CSE future conversions to return the original mutable object.
     *
     * Currently, reading and writing the same file in the same program is not supported, unless there is an alternate dependency chain from the output
     * being written to the input (e.g. the output vector or matrix explicitly depends on the one being read).
     */

    direct (IO) ("readVector", Nil, ("path",MString) :: DenseVector(MDouble)) implements composite ${ readVector[Double]($path, v => optila_todouble(v(0))) }

    direct (IO) ("readMatrix", Nil, ("path", MString) :: DenseMatrix(MDouble)) implements composite ${ readMatrix[Double]($path, s => optila_todouble(s)) }
    direct (IO) ("readMatrix", Nil, (("path", MString), ("delim", MString)) :: DenseMatrix(MDouble)) implements composite ${ readMatrix[Double]($path, s => optila_todouble(s), $delim) }

    val Elem = tpePar("Elem")

    // whitespace delimited by default
    direct (IO) ("readVector", Elem, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> Elem),("delim",MString,"unit(\"\\s+\")")), DenseVector(Elem))) implements composite ${
      val a = ForgeFileReader.readLines($path){ line =>
        val tokens = line.trim.fsplit(delim, -1) // we preserve trailing empty values
        val tokenVector = (0::array_length(tokens)) { i => tokens(i) }
        schemaBldr(tokenVector)
      }
      densevector_fromarray(a, true)
    }

    direct (IO) ("readMatrix", Elem, MethodSignature(List(("path",MString),("schemaBldr",MString ==> Elem),("delim",MString,"unit(\"\\s+\")")), DenseMatrix(Elem))) implements composite ${
      val a = ForgeFileReader.readLinesFlattened($path){ line:Rep[String] =>
        val tokens = line.trim.fsplit(delim, -1) // we preserve trailing empty values
        array_fromfunction(array_length(tokens), i => schemaBldr(tokens(i)))
      }
      val numCols = array_length(readFirstLine(path).trim.fsplit(delim, -1))
      densematrix_fromarray(a, array_length(a) / numCols, numCols)
    }

    val readfirstline = compiler (IO) ("readFirstLine", Nil, ("path",MString) :: MString) implements composite ${
      val in = ForgeFileInputStream(path)
      val line = in.readLine()
      in.close()
      line
    }

    // -- output

    direct (IO) ("writeVector", Elem withBound TStringable, (("v",DenseVector(Elem)),("path",MString)) :: MUnit, effect = simple) implements composite ${
      ForgeFileWriter.writeLines(path, v.length) { i =>
        v(i).makeStr
      }
    }

    direct (IO) ("writeMatrix", Elem withBound TStringable, MethodSignature(List(("m",DenseMatrix(Elem)),("path",MString),("delim",MString,"unit(\"    \")")), MUnit), effect = simple) implements composite ${
      ForgeFileWriter.writeLines(path, m.numRows) { i =>
        array_mkstring(densevector_raw_data(m(i).map(_.makeStr)), delim)
      }
    }


    // -- utility

    direct (IO) ("deleteFile", Nil, MString :: MUnit, effect = simple) implements codegen($cala, ${
      val f = new java.io.File($0)
      if (f.exists) {
        if (f.isDirectory) org.apache.commons.io.FileUtils.deleteDirectory(f) // deletes even if non-empty
        else f.delete()
      }
      ()
    })

  }
}
