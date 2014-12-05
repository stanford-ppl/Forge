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
     * Our current solution is to require the user to explicitly pass dependencies (using "after"), so they can manually force a read to happen after a write,
     * or vice versa.
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
      val a = ForgeFileReader.readLines(path){ line:Rep[String] =>
        val tokens = line.trim.fsplit(delim, -1) // we preserve trailing empty values
        (0::array_length(tokens)) { i => schemaBldr(tokens(i)) }
      }
      DenseMatrix(densevector_fromarray(a, true))

      /*
       * No longer using this version because it forces us to assume the underlying FS to re-open
       * the file (or use Hadoop APIs here), which is not great. However, the version above should be
       * less efficient. We should measure the overhead of constructing the Vector[Vector[]] representation.
       */
      // val a = ForgeFileReader.readLinesFlattened($path){ line:Rep[String] =>
      //   val tokens = line.trim.fsplit(delim, -1) // we preserve trailing empty values
      //   array_fromfunction(array_length(tokens), i => schemaBldr(tokens(i)))
      // }
      // val numCols = array_length(readFirstLine(path).trim.fsplit(delim, -1))
      // densematrix_fromarray(a, array_length(a) / numCols, numCols).unsafeImmutable // unsafeImmutable needed due to struct unwrapping Reflect(Reflect(..)) bug (see LAInputReaderOps.scala line 46 in Delite)
    }

    // val readfirstline = compiler (IO) ("readFirstLine", Nil, ("path",MString) :: MString)

    // impl (readfirstline) (codegen($cala, ${
    //   val xfs = new java.io.BufferedReader(new java.io.FileReader($path))
    //   val line = xfs.readLine()
    //   xfs.close()
    //   line
    // }))

    //NOTE: C++ target codegen for readfirstline uses the Delite C++ library (readFirstLineFile),
    //      but may want to have codegen directly here like scala target (after multi-line C++ codegen in Forge is fixed).
    // impl (readfirstline) (codegen(cpp, ${readFirstLineFile($path)}))


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
