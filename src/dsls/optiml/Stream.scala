package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Stream provides basic iterator functionality over a file or computation too
 * large to fit in memory.
 *
 * Current limitations:
 *   1) HashStream is single-box, Scala-only (uses an embedded DB via codegen methods, lambda for deserialization)
 *   2) FileStream & HashStream are sequential (groupRowsBy would require a lock on the underlying HashStream)
 *   3) FileStream groupRowsBy uses a logical key naming scheme that depends on LevelDB's sorted key storage for efficiency.
 */
trait StreamOps {
  this: OptiMLDSL =>

  def importStreamOps() {
    importHashStreamOps()
    importFileStreamOps()
    importComputeStreamOps()
  }

  def importHashStreamOps() {
    val V = tpePar("V")
    val HashStream = lookupTpe("HashStream")
    val FileStream = lookupTpe("FileStream")
    val Tup2 = lookupTpe("Tup2")

    val LevelDB = tpe("org.iq80.leveldb.DB")
    primitiveTpePrefix ::= "org.iq80"

    data(HashStream, ("_table", MString), ("_db", LevelDB), ("_deserialize", MLambda(Tup2(HashStream(V),MString), V)))

    static (HashStream) ("apply", V, (("table", MString), ("deserialize", (HashStream(V),MString) ==> V)) :: HashStream(V), effect = mutable) implements
      allocates(HashStream, ${$0}, "unit(null.asInstanceOf[org.iq80.leveldb.DB])", ${doLambda((t: Rep[Tup2[HashStream[V],String]]) => deserialize(t._1, t._2))})


    // -- code generated internal methods interface with the embedded db

    compiler (HashStream) ("hash_open_internal", Nil, MString :: LevelDB) implements codegen($cala, ${
      import org.iq80.leveldb._
      import org.fusesource.leveldbjni.JniDBFactory._
      val options = new Options()
      options.createIfMissing(true)
      val db = factory.open(new java.io.File($0), options)
      db
    })

    compiler (HashStream) ("hash_get_internal", Nil, (LevelDB, MString) :: MArray(MByte)) implements codegen($cala, ${
      $0.get(org.fusesource.leveldbjni.JniDBFactory.bytes($1))
    })

    compiler (HashStream) ("hash_put_internal", Nil, (LevelDB, MString, MArray(MByte)) :: MUnit, effect = simple) implements codegen($cala, ${
      $0.put(org.fusesource.leveldbjni.JniDBFactory.bytes($1), $2)
    })

    compiler (HashStream) ("hash_close_internal", Nil, LevelDB :: MUnit, effect = simple) implements codegen($cala, ${
      $0.close()
    })

    compiler (HashStream) ("hash_keys_internal", Nil, LevelDB :: MArray(MString)) implements codegen($cala, ${
      val buf = scala.collection.mutable.ArrayBuffer[String]()
      val iterator = $0.iterator()
      iterator.seekToFirst()

      while (iterator.hasNext) {
        val key = org.fusesource.leveldbjni.JniDBFactory.asString(iterator.peekNext().getKey())
        if (!key.endsWith("_logical")) // skip logical keys
          buf += key
        iterator.next()
      }

      buf.toArray
    })

    // --

    val HashStreamOps = withTpe(HashStream)
    HashStreamOps {
      compiler ("hash_deserialize") (Nil :: MLambda(Tup2(HashStream(V),MString), V)) implements getter(0, "_deserialize")
      compiler ("hash_table_name") (Nil :: MString) implements getter(0, "_table")
      compiler ("hash_get_db") (Nil :: LevelDB) implements getter(0, "_db")
      compiler ("hash_set_db") (LevelDB :: MUnit, effect = write(0)) implements setter(0, "_db", ${$1})

      infix ("open") (Nil :: MUnit, effect = write(0)) implements single ${
        val table = hash_table_name($self)
        val db = hash_open_internal(table)
        hash_set_db($self, db)
      }

      infix ("apply") (MString :: V) implements composite ${
        val lambda = hash_deserialize($self)
        doApply(lambda, pack(($self,$1)))
      }

      infix ("get") (MString :: MArray(MByte)) implements composite ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_get_internal(db, $1)
      }

      infix ("put") ((MString, MArray(MByte)) :: MUnit, effect = write(0)) implements single ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_put_internal(db, $1, $2)
      }

      infix ("close") (Nil :: MUnit, effect = write(0)) implements single ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_close_internal(db)
        hash_set_db($self, unit(null.asInstanceOf[org.iq80.leveldb.DB]))
      }


      // -- bulk

      infix ("mapValues") (CurriedMethodSignature(List(
        List(
          ("outFile", MString)
        ),
        List(
          ("func", V ==> MString)
        )), FileStream)) implements composite ${

        val out = ForgeFileOutputStream(outFile)

        // This requires 2 passes (one to get the keys, the next to process the values),
        // because we cannot call our deserialize function from within a codegen method.
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")

        val lambda = hash_deserialize($self)

        val keys = hash_keys_internal(db)
        var i = 0
        while (i < keys.length) {
          val v = doApply(lambda, pack(($self,keys(i))))
          out.writeLine(func(v))
          i += 1
        }
        out.close()

        FileStream(outFile)
      }
    }
  }

  def importFileStreamOps() {
    val FileStream = lookupTpe("FileStream")
    val HashStream = lookupTpe("HashStream")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    data(FileStream, ("_path", MString))

    static (FileStream) ("apply", Nil, MString :: FileStream, effect = simple) implements allocates(FileStream, ${$0})

    val T = tpePar("T")
    val R = tpePar("R")

    // we need to read sequentially, but from potentially different data stores, so we use ForgeFileInputStream and ForgeFileOutputStream
    // TODO: We should load a chunk in at a time in parallel, and write the chunk out in parallel, and then read the next chunk (like ComputeStream).
    //       To do this we need to extend DeliteFileReader and DeliteFileWriter to handle fixed size loops.
    val FileStreamOps = withTpe(FileStream)
    FileStreamOps {
      infix ("path") (Nil :: MString) implements getter(0, "_path")

      // currently loaded and executed sequentially, chunk-by-chunk
      infix ("foreach") ((MString ==> MUnit) :: MUnit, effect = simple) implements single ${
        val f = ForgeFileInputStream($self.path)
        var line = f.readLine()
        while (line != null) {
          $1(line)
          line = f.readLine()
        }
        f.close()
      }

      infix ("map") (CurriedMethodSignature(List(
        List(
          ("outFile", MString)
        ),
        List(
          ("func", MString ==> MString)
        )), FileStream)) implements composite ${

        val out = ForgeFileOutputStream(outFile)
        // the below incorrectly infers the type of 'line' for mysterious reasons
        // for (line <- $self) {
        $self.foreach { line: Rep[String] =>
          out.writeLine(func(line))
        }
        out.close()

        FileStream(outFile)
      }

      infix ("mapRows") (CurriedMethodSignature(List(
        List(
          ("outFile", MString),
          ("inDelim", MString, "unit(\"\\s+\")"),   // input delimiter is a regular expression
          ("outDelim", MString, "unit(\"    \")") // output delimiter is an ordinary string
        ),
        List(
          ("func", DenseVector(MString) ==> DenseVector(R))
        )), FileStream), TStringable(R), addTpePars = R) implements composite ${

        val out = ForgeFileOutputStream(outFile)
        $self.foreach { line: Rep[String] =>
          // explicit types are needed because of some scalac bug (not found tokenVector, outRow)
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(inDelim, -1) // we preserve trailing empty values
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          val outRow: Rep[DenseVector[String]] = func(tokenVector) map { e => e.makeStr }
          val outLine: Rep[String] = array_mkstring(densevector_raw_data(outRow), outDelim)
          if (outLine != "")
            out.writeLine(outLine)
        }
        out.close()

        FileStream(outFile)
      }

      // We currently limit this to (String, DenseVector(Double)) pairs, as we need to be able to serialize
      // the representation out to disk efficiently. We could allow the user to supply serializers and
      // deserializers (or use a serializable type class) but that may be more trouble than it is worth.
      // We could also fall back to just using strings, but that is painfully slow.
      infix ("groupRowsBy") (CurriedMethodSignature(List(
        List(
          ("outTable", MString),
          ("delim", MString, "unit(\"\\s+\")")   // input delimiter is a regular expression
        ),
        List(
          ("keyFunc", DenseVector(MString) ==> MString),
          ("valFunc", DenseVector(MString) ==> DenseVector(MDouble))
        )), HashStream(DenseMatrix(MDouble)))) implements composite ${

        def serialize(v: Rep[DenseVector[Double]]): Rep[ForgeArray[Byte]] = {
          val x = ByteBuffer(4+8*v.length)
          x.putInt(v.length)
          x.put(v.toArray, 0, v.length)
          x.array
        }

        def deserialize(hash: Rep[HashStream[DenseMatrix[Double]]], k: Rep[String]): Rep[DenseMatrix[Double]] = {
          val a: Rep[ForgeArray[Byte]] = hash.get(k)
          fassert(a != null, "HashStream: could not find key " + k)
          fassert(a.length == 4, "HashStream: lookup of key " + k + " did not return an integer value (length) as expected") // should be the number of rows in this group

          val x = ByteBufferWrap(a)
          val numRows = x.getInt()

          // process first row so we can preallocate
          val firstRow = hash.get(k + "_" + 0 + "_logical")
          val firstBuffer = ByteBufferWrap(firstRow)
          val numCols = firstBuffer.getInt()
          val out = DenseMatrix[Double](numRows,numCols)

          var i = 0
          while (i < numRows) {
            val rowArray = hash.get(k + "_" + i + "_logical")
            val rowBuffer = ByteBufferWrap(rowArray)
            val numCols = rowBuffer.getInt()
            val dst = densematrix_raw_data(out).unsafeMutable // array_update gets rewritten, but not the copy below
            rowBuffer.unsafeImmutable.get(dst, i*numCols, numCols) // write directly to underlying matrix
            i += 1
          }
          out.unsafeImmutable
        }

        deleteFile(outTable)
        val hash = HashStream[DenseMatrix[Double]](outTable, deserialize)
        hash.open()

        $self.foreach { line: Rep[String] =>
          // explicit types are needed because of some scalac bug (not found tokenVector, outRow)
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(delim, -1) // we preserve trailing empty values
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          val key: Rep[String] = keyFunc(tokenVector)
          val value: Rep[DenseVector[Double]] = valFunc(tokenVector)

          // The scheme belows appends new rows as new logical keys (instead of storing and growing a byte array
          // value in the "master" key). This relies on LevelDBs sorted key functionality for efficiency (the logical
          // keys are stored close together, enabling compression and sequential scanning).
          //
          // However, we want to process multiple rows in parallel and do a batch append, but this is not
          // thread-safe. There is no atomic get-modify-put operation, so multiple parallel readers can end
          // up generating duplicate logical keys, unless we use our own concurrency control.
          if (value.length > 0) {
            val existing: Rep[ForgeArray[Byte]] = hash.get(key)
            val numRows: Rep[Int] = if (existing == null) 0 else ByteBufferWrap(existing).getInt()
            val newRows: Rep[Int] = numRows+1
            val lenBuf: Rep[java.nio.ByteBuffer] = ByteBuffer(4)
            lenBuf.putInt(newRows)
            hash.put(key, lenBuf.array)
            val logicalKey = key + "_" + numRows + "_logical" // logical keys are 0-indexed
            hash.put(logicalKey, serialize(value))
          }
        }

        hash
      }

      infix ("reduce") (CurriedMethodSignature(List(List(("init", T)), List(("func", (T,MString) ==> T))), T), addTpePars = T) implements composite ${
        var acc = init
        // for (line <- $self) {
        $self.foreach { line: Rep[String] =>
          acc = func(acc, line)
        }
        acc
      }
    }
  }

  def importComputeStreamOps() {
    val T = tpePar("T")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val Tup2 = lookupTpe("Tup2")
    val ComputeStream = lookupTpe("ComputeStream")

    data(ComputeStream, ("_numRows", MInt), ("_numCols", MInt), ("_func", MLambda(Tup2(MInt,MInt), T)))

    static (ComputeStream) ("apply", T, (CurriedMethodSignature(List(
      List(
        ("numRows", MInt),
        ("numCols", MInt)
      ),
      List(
        ("func", (MInt,MInt) ==> T)
      )), ComputeStream(T)))) implements allocates(ComputeStream, ${$0}, ${$1}, ${doLambda((t: Rep[Tup2[Int,Int]]) => func(t._1, t._2))})

    val ComputeStreamOps = withTpe(ComputeStream)
    ComputeStreamOps {
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      compiler ("stream_func") (Nil :: MLambda(Tup2(MInt,MInt), T)) implements getter(0, "_func")

      infix ("apply") ((MInt, MInt) :: T) implements composite ${
        val lambda = stream_func($self)
        doApply(lambda, pack(($1,$2)))
      }

      infix ("foreach") ((T ==> MUnit) :: MUnit) implements composite ${
        (0::$self.numRows) foreach { i =>
          // the below produces a could not find "::" value error for mysterious reasons
          // (0::$self.numCols) foreach { j =>
          IndexVector(0, $self.numCols) foreach { j =>
            $1($self(i,j))
          }
        }
      }

      infix ("foreachRow") ((DenseVectorView(T) ==> MUnit) :: MUnit) implements composite ${
        // buffered to avoid producing a large amount of garbage (instantiating a row each time)
        // we use a matrix instead of a single vector as a buffer to increase parallelism
        val chunkSize = ceil(1000000/$self.numCols) // heuristic for number of rows to process at one time. total buffer size is chunkSize x numCols
        val buf = DenseMatrix[T](chunkSize, $self.numCols)
        val numChunks = ceil($self.numRows / chunkSize.toDouble)

        var chunkIdx = 0
        while (chunkIdx < numChunks) {
          val remainingRows = $self.numRows - chunkIdx*chunkSize
          val leftover = if (remainingRows < 0) $self.numRows else remainingRows // in case numRows < chunkSize
          val rowsToProcess = min(chunkSize, leftover)
          (0::rowsToProcess) foreach { i =>
            // (0::$self.numCols) foreach { j =>
            IndexVector(0, $self.numCols) foreach { j =>
              buf(i,j) = $self(i,j)
            }
            $1(buf(i))
          }

          chunkIdx += 1
        }
      }
    }
  }
}
