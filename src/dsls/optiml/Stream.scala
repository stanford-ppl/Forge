package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Stream provides basic iterator functionality over a file or computation too
 * large to fit in memory.
 *
 * Note: HashStream's mapValues and FileStream's map/mapRows will both skip writing empty
 *       lines / rows to file, which allows for in-line filtering of bad records.
 *
 * Current limitations:
 *   1) HashStream is single-box, Scala-only (uses an embedded DB via codegen methods, lambda for deserialization)
 *   2) FileStream groupRowsBy uses a logical key naming scheme that depends on LevelDB's sorted key storage for efficiency.
 */
trait StreamOps {
  this: OptiMLDSL =>

  /* we internally have a Key -> Coll[Value] store, implemented as a key := LogicalKey_Sep_UniqueId -> Value
   * This is done so that a logical append corresponds to only a physical put rather than a get-concat-put
   * LevelDB sorts data by key, so using the logical key as a prefix makes LevelDB keep all Coll[Value] adjacent on disk
   */
  val HASH_LOGICAL_KEY_SEPARATOR = "_LK_"

  def importStreamOps() {
    importHashStreamOps()
    importFileStreamOps()
    importComputeStreamOps()
  }

  def importHashStreamOps() {
    val HashStream = lookupTpe("HashStream")
    val FileStream = lookupTpe("FileStream")
    val DenseVector = lookupTpe("DenseVector")
    val KeyValueStore = lookupTpe("KeyValueStore")
    val Tup2 = lookupTpe("Tup2")
    val V = tpePar("V")
    val R = tpePar("R")

    data(HashStream, ("_db", KeyValueStore(V)))

    static (HashStream) ("apply", V, (("table", MString), ("deserialize", (HashStream(V),MString) ==> V)) :: HashStream(V), effect = mutable) implements composite ${
      val wrappedDeserialize = (a: Rep[KeyValueStore[V]], b: Rep[String]) => deserialize(hash_alloc_raw(a), b)
      val hash = hash_alloc_raw[V](kv_alloc_raw[V](table, wrappedDeserialize))
      hash.open()
      hash
    }

    compiler (HashStream) ("hash_alloc_raw", V, KeyValueStore(V) :: HashStream(V)) implements allocates(HashStream, ${$0})

    // --

    val HashStreamOps = withTpe(HashStream)
    HashStreamOps {
      compiler ("hash_get_db") (Nil :: KeyValueStore(V)) implements getter(0, "_db")
      //compiler ("hash_set_db") (KeyValueStore(V) :: MUnit, effect = write(0)) implements setter(0, "_db", ${$1})
      compiler ("hash_deserialize") (Nil :: MLambda(Tup2(HashStream(V),MString), V)) implements composite ${
        val d = kv_deserialize(hash_get_db($self))
        doLambda((t: Rep[Tup2[HashStream[V],String]]) => d(pack(hash_get_db(t._1), t._2)))
      }

      infix ("open") (Nil :: MUnit, effect = write(0)) implements composite ${ hash_get_db($self).open() }

      infix ("apply") (MString :: V) implements composite ${ hash_get_db($self).apply($1) }

      infix ("contains") (MString :: MBoolean) implements composite ${ hash_get_db($self).contains($1) }

      infix ("keys") (Nil :: MArray(MString)) implements composite ${ hash_get_db($self).keys }

      infix ("get") (MString :: MArray(MByte)) implements composite ${ hash_get_db($self).get($1) }

      infix ("put") ((MString, MArray(MByte)) :: MUnit, effect = write(0)) implements composite ${ hash_get_db($self).put($1, $2) }

      //get all values associated with the supplied logical key (prefix)
      infix ("getAll") (MString :: MArray(MArray(MByte))) implements composite ${ hash_get_db($self).getAll($1) }

      infix ("putAll") ((MArray(MString), MArray(MString), MArray(MArray(MByte)), MInt) :: MUnit, effect = write(0)) implements composite ${ hash_get_db($self).putAll($1,$2,$3,$4) }

      infix ("close") (Nil :: MUnit, effect = write(0)) implements composite ${ hash_get_db($self).close() }

      // -- bulk

      infix ("mapValues") (CurriedMethodSignature(List(
        List(
          ("outFile", MString),
          ("outDelim", MString, "unit(\"    \")") // output delimiter is an ordinary string
        ),
        List(
          ("func", (MString,V) ==> DenseVector(R))
        )), FileStream), TStringable(R), addTpePars = R) implements composite ${

        // Delete destination if it exists
        deleteFile(outFile)

        // This requires 2 passes (one to get the keys, the next to process the values),
        // because we cannot call our deserialize function from within a codegen method.
        val lambda = hash_deserialize($self)
        val keys = $self.keys

        // Performance is particularly sensitive to chunkSize because currently this function is very susceptible to load
        // balance issues (i.e. some keys are associated with much more data and therefore execution time). We should address
        // this by computing key sizes ahead-of-time and then splitting the work in a more balanced way.
        val chunkSize = 10000000 // getting better results with a constant, for now. this is highly dependent on the application.
        // val chunkSize = (getChunkByteSize / 1000).toInt // number of keys to process in parallel. assume each key holds ~1KB.

        val numChunks = ceil(keys.length.toDouble / chunkSize)
        var i = 0
        var keysProcessed = 0
        while (i < numChunks) {
          // process remainder if we're the last chunk
          val processSize: Rep[Int] = if (i == numChunks - 1) keys.length - keysProcessed else chunkSize
          val vecs = array_fromfunction(processSize, i => func(keys(keysProcessed+i), doApply(lambda, pack(($self,keys(keysProcessed+i))))))
          val writeArray = array_filter(vecs, (e: Rep[DenseVector[R]]) => e.length > 0)
          ForgeFileWriter.writeLines(outFile, writeArray.length, append = true) { i =>
            val v = writeArray(i)
            array_mkstring(v.toArray, outDelim)
          }
          keysProcessed += chunkSize
          i += 1
        }

        FileStream(outFile)
      }
    }
  }

  def importFileStreamOps() {
    val FileStream = lookupTpe("FileStream")
    val HashStream = lookupTpe("HashStream")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val Tup2 = lookupTpe("Tup2")

    data(FileStream, ("_path", MString))

    static (FileStream) ("apply", Nil, MString :: FileStream, effect = simple) implements allocates(FileStream, ${$0})

    val T = tpePar("T")
    val R = tpePar("R")

    // These "hashMatrix" operations are factored out of groupRowsBy() so that end users can create a
    // HashStream directly from a persistent hash that was constructed from a FileStream.groupRowsBy() call.
    //     e.g. val hash = HashStream("test.hash", hashMatrixDeserializer)
    direct (FileStream) ("hashMatrixDeserializer", Nil, (("hash", HashStream(DenseMatrix(MDouble))), ("k", MString)) :: DenseMatrix(MDouble)) implements composite ${
      val rows = hash.getAll(k)
      if (rows == null) {
        unit(null).AsInstanceOf[DenseMatrix[Double]]
      }
      else {
        val numRows = rows.length
        val numCols0 = ByteBufferWrap(rows(0)).getInt()
        val out = DenseMatrix[Double](numRows, numCols0)

        var i = 0
        while (i < numRows) {
          val rowArray = rows(i)
          val rowBuffer = ByteBufferWrap(rowArray)
          val numCols = rowBuffer.getInt()

          fassert(numCols0 == numCols, "hashMatrixDeserializer: expected " + numCols0 + " cols for row " + i + ", but found " + numCols)
          val dst = densematrix_raw_data(out).unsafeMutable // array_update gets rewritten, but not the write below
          rowBuffer.unsafeImmutable.get(dst, i*numCols, numCols) // write directly to underlying matrix
          i += 1
        }

        out.unsafeImmutable
      }
    }

    // Create a unique key suffix for the given record
    compiler (FileStream) ("hashMatrixKeySuffix", Nil, MArray(MByte) :: MString) implements codegen($cala, ${
      val uniqueId = com.google.common.hash.Hashing.murmur3_128.hashBytes($0)
      uniqueId.toString
    })

    // --

    direct (FileStream) ("getChunkByteSize", Nil, Nil :: MLong) implements composite ${
      // While this seems very conservative, with a 100 GB heap on a 128 GB machine, larger sizes blow out the heap.
      // (0.10*getMaxHeapSize).toLong

      // A reasonable and static (e.g. 1 GB) chunk size seems to give us the most consistent good performance. This should be configurable.
      // Using a constant also seems to increase fusion opportunities, though what is happening is a bit of a mystery.
      // without the explicit types the results gets lifted and 'toX' is staged :(
      val chunkSize: String = System.getProperty("optiml.stream.chunk.bytesize","1e9")
      val chunkDouble: Double = chunkSize.toDouble
      unit(chunkDouble.toLong)
    }

    // We need to read sequentially, but from potentially different data stores, so we use ForgeFileInputStream and ForgeFileOutputStream
    val FileStreamOps = withTpe(FileStream)
    FileStreamOps {
      infix ("path") (Nil :: MString) implements getter(0, "_path")

      // rows are loaded and executed sequentially
      infix ("foreach") ((MString ==> MUnit) :: MUnit, effect = simple) implements single ${
        val f = ForgeFileInputStream($self.path)
        var line = f.readLine()
        while (line != null) {
          $1(line)
          line = f.readLine()
        }
        f.close()
      }

      infix ("processFileChunks") (MethodSignature(List(("readFunc", (MString,MString) ==> R), ("processFunc", MArray(R) ==> MUnit), ("chunkSize", MLong, "filestream_getchunkbytesize()")), MUnit), addTpePars = R) implements composite ${
        val f = ForgeFileInputStream($self.path)
        val totalSize = f.size

        val numChunks = ceil(totalSize.toDouble / chunkSize)
        var totalBytesRead = 0L
        var totalLinesRead = 0
        var i = 0

        val verbose = System.getProperty("optiml.stream.verbose", "false").toBoolean
        val granularity =
          if ((numChunks / 1000000.0) > 10.0) 1000000
          else if ((numChunks / 100000.0) > 10.0) 100000
          else if ((numChunks / 10000.0) > 10.0) 10000
          else if ((numChunks / 1000.0) > 10.0) 1000
          else if ((numChunks / 100.0) > 10.0) 100
          else if ((numChunks / 10.0) > 10.0) 10
          else 1

        while (i < numChunks) {
          if (verbose && ((i % granularity) == 0)) {
            println("[" + i + " / " + numChunks + "]")
          }
          // process remainder if we're the last chunk
          val processSize: Rep[Long] = if (i == numChunks - 1) totalSize - totalBytesRead else chunkSize
          val a = ForgeFileReader.readLinesChunk(f)(totalBytesRead, processSize)(readFunc)
          processFunc(a)

          totalBytesRead += processSize
          totalLinesRead += a.length
          i += 1
        }

        f.close()
      }

      // chunks are loaded in parallel, one chunk at a time
      infix ("map") (CurriedMethodSignature(List(
        List(
          ("outFile", MString),
          ("preserveOrder", MBoolean, "unit(false)"),
          ("chunkSize", MLong, "filestream_getchunkbytesize()")
        ),
        List(
          ("func", MString ==> MString)
        )), FileStream)) implements composite ${

        // Delete destination if it exists
        deleteFile(outFile)

        $self.processFileChunks((a,b) => func(a), { (a: Rep[ForgeArray[String]]) =>
          if (!preserveOrder) {
            // This only makes sense if the order of the writes doesn't matter, since we are striping each chunk across the
            // different output files. Therefore when they are reconstructed, output file 0 will have elements from all chunks,
            // instead of just the first n elements.
            val writeArray = array_filter(a, (e: Rep[String]) => e.length > 0)
            ForgeFileWriter.writeLines(outFile, writeArray.length, append = true)(i => writeArray(i))
          }
          else {
            // write sequentially
            val out = ForgeFileOutputStream(outFile, append = true)
            for (j <- 0 until a.length) {
              if (a(j).length > 0)
                out.writeLine(a(j))
            }
            out.close()
          }
        }, chunkSize)

        FileStream(outFile)
      }

      infix ("mapRows") (CurriedMethodSignature(List(
        List(
          ("outFile", MString),
          ("inDelim", MString, "unit(\"\\s+\")"), // input delimiter is a regular expression
          ("outDelim", MString, "unit(\"    \")"), // output delimiter is an ordinary string
          ("trim", MBoolean, "unit(true)")
        ),
        List(
          ("func", DenseVector(MString) ==> DenseVector(R))
        )), FileStream), TStringable(R), addTpePars = R) implements composite ${

        $self.map(outFile, preserveOrder = true) { line =>
          val input = if (trim) line.trim else line
          val tokens: Rep[ForgeArray[String]] = input.fsplit(inDelim, -1) // we preserve trailing empty values
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          // val outRow: Rep[DenseVector[String]] = func(tokenVector) map { e => e.makeStr }
          // val outLine: Rep[String] = array_mkstring(densevector_raw_data(outRow), outDelim)
          val outRow: Rep[DenseVector[R]] = func(tokenVector)
          // skip OptiLA formatting (slow) when writing to file... this is a little sketchy.
          if (outRow.length > 0) array_mkstring(outRow.toArray, outDelim) else ""
        }

        FileStream(outFile)
      }

      // We currently limit this to (String, DenseVector(Double)) pairs, as we need to be able to serialize
      // the representation out to disk efficiently. We could allow the user to supply serializers and
      // deserializers (or use a serializable type class) but that may be more trouble than it is worth.
      // We could also fall back to just using strings, but that is painfully slow.
      infix ("groupRowsBy") (CurriedMethodSignature(List(
        List(
          ("outTable", MString),
          ("delim", MString, "unit(\"\\s+\")"),   // input delimiter is a regular expression
          ("trim", MBoolean, "unit(true)"),
          ("appendToHash", MBoolean, "unit(false)")
        ),
        List(
          ("keyFunc", DenseVector(MString) ==> MString),
          ("valFunc", DenseVector(MString) ==> DenseVector(MDouble))
        )), HashStream(DenseMatrix(MDouble)))) implements composite ${

        def serialize(v: Rep[DenseVector[Double]]): Rep[ForgeArray[Byte]] = {
          val x = ByteBuffer(4+8*v.length)
          x.putInt(v.length)
          x.put(v.toArray, 0, v.length)
          x.unsafeImmutable.array
        }

        /* see hashMatrixDeserializer for deserialization */

        if (!appendToHash) {
          deleteFile(outTable) // Delete destination if it exists
        }
        val hash = HashStream[DenseMatrix[Double]](outTable, hashMatrixDeserializer)

        $self.processFileChunks({ (line, location) =>
          val input = if (trim) line.trim else line
          val tokens: Rep[ForgeArray[String]] = input.fsplit(delim, -1) // we preserve trailing empty values
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          val key: Rep[String] = keyFunc(tokenVector)
          val value: Rep[DenseVector[Double]] = valFunc(tokenVector)
          pack((key, value, location))
        },
        { (a: Rep[ForgeArray[Tup3[String,DenseVector[Double],String]]]) =>
          // The scheme belows appends new rows as new keys (instead of reading and growing the existing byte array)
          // This relies on LevelDBs sorted key functionality for efficiency
          // (the logical keys are stored close together, enabling compression and sequential scanning).
          //
          // We write to the map sequentially to avoid needing to use external concurrency control (and because benchmarks
          // do not show LevelDB scaling well with concurrent writers). We also batch all of the writes in one chunk for
          // improved performance.

          // Zero-length rows are skipped (note that the only two valid lengths for "value" are 0 and numCols)
          // If there is a key present in the resulting map, there was at least 1 non-empty row mapping to it.
          val chunk = densevector_fromarray(a, true).filter(t => t._2.length > 0)

          val dbKeyPrefix: Rep[ForgeArray[String]] = chunk.map(t => t._1).toArray
          val dbKeySuffix: Rep[ForgeArray[String]] = chunk.map(t => t._3).toArray
          val dbValues: Rep[ForgeArray[ForgeArray[Byte]]] = chunk.map(t => serialize(t._2)).toArray

          // We seem to be getting write bandwidth on the logicalKeys of ~15MB/sec, while the Google benchmarks at
          // https://github.com/google/leveldb claim writes should be ~45MB/sec. This could be due to hardware, JNI,
          // or the Java driver.
          hash.putAll(dbKeyPrefix, dbKeySuffix, dbValues, dbKeyPrefix.length)
        })

        hash
      }

      infix ("reduce") (CurriedMethodSignature(List(List(("zero", T)), List(("func", MString ==> T)), List(("rfunc", (T,T) ==> T))), T), addTpePars = T) implements composite ${
        var acc = zero

        $self.processFileChunks((line, location) => func(line), { (a: Rep[ForgeArray[T]]) =>
          val reduced = array_reduce(a, rfunc, zero)
          acc = rfunc(acc, reduced)
        })

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
