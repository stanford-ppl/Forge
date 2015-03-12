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

  val HASH_LOGICAL_KEY_PREFIX = "_LK_"

  def importStreamOps() {
    importHashStreamOps()
    importFileStreamOps()
    importComputeStreamOps()
  }

  def importHashStreamOps() {
    val HashStream = lookupTpe("HashStream")
    val FileStream = lookupTpe("FileStream")
    val DenseVector = lookupTpe("DenseVector")
    val Tup2 = lookupTpe("Tup2")
    val V = tpePar("V")
    val R = tpePar("R")

    val LevelDB = tpe("org.iq80.leveldb.DB")
    primitiveTpePrefix ::= "org.iq80"

    data(HashStream, ("_table", MString), ("_db", LevelDB), ("_deserialize", MLambda(Tup2(HashStream(V),MString), V)))

    static (HashStream) ("apply", V, (("table", MString), ("deserialize", (HashStream(V),MString) ==> V)) :: HashStream(V), effect = mutable) implements composite ${
      val hash = hash_alloc_raw[V](table, deserialize)
      hash.open()
      hash
    }

    compiler (HashStream) ("hash_alloc_raw", V, (("table", MString), ("deserialize", (HashStream(V),MString) ==> V)) :: HashStream(V), effect = mutable) implements
      allocates(HashStream, ${$0}, "unit(null.asInstanceOf[org.iq80.leveldb.DB])", ${doLambda((t: Rep[Tup2[HashStream[V],String]]) => deserialize(t._1, t._2))})


    // -- code generated internal methods interface with the embedded db

    // We use simple effects in lieu of read / write effects because these are codegen nodes,
    // so we cannot pass the struct to them (a limitation of Forge at the moment).

    compiler (HashStream) ("hash_open_internal", Nil, MString :: LevelDB, effect = simple) implements codegen($cala, ${
      import org.iq80.leveldb._
      import org.fusesource.leveldbjni.JniDBFactory._
      val options = new Options()
      options.createIfMissing(true)
      // options.cacheSize(100000000L)
      val db = factory.open(new java.io.File($0), options)
      db
    })

    compiler (HashStream) ("hash_get_internal", Nil, (LevelDB, MArray(MByte)) :: MArray(MByte), effect = simple) implements codegen($cala, ${
      $0.get($1)
    })

    compiler (HashStream) ("hash_get_range_internal", Nil, (LevelDB, MArray(MByte), MInt) :: MArray(MArray(MByte)), effect = simple) implements codegen($cala, ${
      // workaround for named arguments in codegen methods not working
      val db = $0
      val startKey = $1
      val numKeys = $2

      val buf = scala.collection.mutable.ArrayBuffer[Array[Byte]]()
      val iterator = db.iterator()
      iterator.seek(startKey)

      var pos = 0
      while (iterator.hasNext && pos < numKeys) {
        // val key = org.fusesource.leveldbjni.JniDBFactory.asString(iterator.peekNext().getKey())
        buf += iterator.peekNext().getValue()
        iterator.next()
        pos += 1
      }

      iterator.close()
      buf.toArray
    })

    compiler (HashStream) ("hash_put_internal", Nil, (LevelDB, MArray(MByte), MArray(MByte)) :: MUnit, effect = simple) implements codegen($cala, ${
      $0.put($1, $2)
    })

    compiler (HashStream) ("hash_put_all_internal", Nil, (LevelDB, MArray(MArray(MByte)), MArray(MArray(MByte)), MInt) :: MUnit, effect = simple) implements codegen($cala, ${
      assert($1.length >= $3 && $2.length >= $3, "HashStream putAll called with too small arrays")
      val batch = $0.createWriteBatch()
      var i = 0
      while (i < $3) {
        batch.put($1(i), $2(i))
        i += 1
      }
      $0.write(batch)
      batch.close()
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
        if (!key.startsWith("\$HASH_LOGICAL_KEY_PREFIX")) // skip logical keys
          buf += key
        iterator.next()
      }

      iterator.close()
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

      // This may be too inefficient, since a subsequent get has to hit the hash again.
      // However, if it's cached, it should be fine.
      infix ("contains") (MString :: MBoolean) implements single ${
        $self.get($1) != null
      }

      infix ("keys") (Nil :: MArray(MString)) implements single ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_keys_internal(db)
      }

      infix ("get") (MString :: MArray(MByte)) implements single ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_get_internal(db, $1.getBytes)
      }

      infix ("getRange") ((MString, MInt) :: MArray(MArray(MByte))) implements single ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_get_range_internal(db, $1.getBytes, $2)
      }

      infix ("put") ((MString, MArray(MByte)) :: MUnit, effect = write(0)) implements single ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_put_internal(db, $1.getBytes, $2)
      }

      infix ("putAll") ((MArray(MString), MArray(MArray(MByte)), MInt) :: MUnit, effect = write(0)) implements composite ${
        val db = hash_get_db($self)
        fassert(db != null, "No DB opened in HashStream")
        hash_put_all_internal(db, $1.map(_.getBytes), $2, $3)
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

    data(FileStream, ("_path", MString))

    static (FileStream) ("apply", Nil, MString :: FileStream, effect = simple) implements allocates(FileStream, ${$0})

    val T = tpePar("T")
    val R = tpePar("R")

    // These "hashMatrix" operations are factored out of groupRowsBy() so that end users can create a
    // HashStream directly from a persistent hash that was constructed from a FileStream.groupRowsBy() call.
    //     e.g. val hash = HashStream("test.hash", hashMatrixDeserializer)
    direct (FileStream) ("hashMatrixDeserializer", Nil, (("hash", HashStream(DenseMatrix(MDouble))), ("k", MString)) :: DenseMatrix(MDouble)) implements composite ${
      val a: Rep[ForgeArray[Byte]] = hash.get(k)
      fassert(a != null, "HashStream: could not find key " + k)
      fassert(a.length == 4, "HashStream: lookup of key " + k + " did not return an integer value (length) as expected") // should be the number of rows in this group

      val firstKey = hashMatrixLogicalKey(k, 0)
      val numRows = ByteBufferWrap(a).getInt()
      fassert(numRows > 0, "HashStream: attempted to deserialize a matrix key with 0 rows which is unexpected")
      val rows = hash.getRange(firstKey, numRows)
      val numCols = ByteBufferWrap(rows(0)).getInt()

      val out = DenseMatrix[Double](numRows, numCols)

      var i = 0
      while (i < numRows) {
        val rowArray = rows(i)
        val rowBuffer = ByteBufferWrap(rowArray)
        val numCols = rowBuffer.getInt()
        val dst = densematrix_raw_data(out).unsafeMutable // array_update gets rewritten, but not the write below
        rowBuffer.unsafeImmutable.get(dst, i*numCols, numCols) // write directly to underlying matrix
        i += 1
      }

      out.unsafeImmutable
    }

    // Create a lexicographically ordered logical key that respects integer order. We do this instead of
    // supplying a custom comparator to LevelDB, which would require jumping across the JNI boundary to invoke.
    compiler (FileStream) ("hashMatrixLogicalKey", Nil, (("k", MString), ("index", MInt)) :: MString) implements composite ${
      val prefix = "\$HASH_LOGICAL_KEY_PREFIX" + k + "_"
      val strIndex = ""+index
      val suffix = strIndex.length + strIndex
      prefix + suffix
    }

    // --

    direct (FileStream) ("getChunkByteSize", Nil, Nil :: MLong) implements composite ${
      // While this seems very conservative, with a 100 GB heap on a 128 GB machine, larger sizes blow out the heap.
      // (0.10*getMaxHeapSize).toLong

      // A reasonable and static (e.g. 1 GB) chunk size seems to give us the most consistent good performance. This should be configurable.
      // Using a constant also seems to increase fusion opportunities, though what is happening is a bit of a mystery.
      (1e9).toLong
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

      compiler ("processFileChunks") (MethodSignature(List(("readFunc", MString ==> R), ("processFunc", MArray(R) ==> MUnit), ("chunkSize", MLong, "filestream_getchunkbytesize()")), MUnit), addTpePars = R) implements composite ${
        val f = ForgeFileInputStream($self.path)
        val totalSize = f.size
        f.close()

        val numChunks = ceil(totalSize.toDouble / chunkSize)
        var totalBytesRead = 0L
        var totalLinesRead = 0
        var i = 0

        while (i < numChunks) {
          // process remainder if we're the last chunk
          val processSize: Rep[Long] = if (i == numChunks - 1) totalSize - totalBytesRead else chunkSize
          val a = ForgeFileReader.readLinesChunk($self.path)(totalBytesRead, processSize)(readFunc)
          processFunc(a)

          totalBytesRead += processSize
          totalLinesRead += a.length
          i += 1
        }
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

        processFileChunks($self, func, { (a: Rep[ForgeArray[String]]) =>
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
          ("outDelim", MString, "unit(\"    \")") // output delimiter is an ordinary string
        ),
        List(
          ("func", DenseVector(MString) ==> DenseVector(R))
        )), FileStream), TStringable(R), addTpePars = R) implements composite ${

        $self.map(outFile) { line =>
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(inDelim, -1) // we preserve trailing empty values
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

        processFileChunks($self, { line =>
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(delim, -1) // we preserve trailing empty values
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          val key: Rep[String] = keyFunc(tokenVector)
          val value: Rep[DenseVector[Double]] = valFunc(tokenVector)
          pack((key, value))
        },
        { (a: Rep[ForgeArray[Tup2[String,DenseVector[Double]]]]) =>
          // The scheme belows appends new rows as new logical keys (instead of storing and growing a byte array
          // value in the "master" key). This relies on LevelDBs sorted key functionality for efficiency (the logical
          // keys are stored close together, enabling compression and sequential scanning).
          //
          // We write to the map sequentially to avoid needing to use external concurrency control (and because benchmarks
          // do not show LevelDB scaling well with concurrent writers). We also batch all of the writes in one chunk for
          // improved performance.

          // Zero-length rows are skipped (note that the only two valid lengths for "value" are 0 and numCols)
          // If there is a key present in the resulting map, there was at least 1 non-empty row mapping to it.
          val chunk = densevector_fromarray(a, true).filter(v => v._2.length > 0)
          val chunkRows: Rep[ForgeHashMap[String, DenseVector[DenseVector[Double]]]] = chunk.groupBy(v => v._1, v => v._2)

          val allKeys: Rep[DenseVector[String]] = densevector_fromarray(chunkRows.keys, true)
          val allValues: Rep[DenseVector[DenseVector[DenseVector[Double]]]] = densevector_fromarray(fhashmap_values(chunkRows), true)
          val allKV = allKeys.zip(allValues) { (k,v) => pack((k,v)) }

          val masterValues: Rep[ForgeArray[ForgeArray[Byte]]] =
            allValues.map { group =>
              val lenBuf: Rep[java.nio.ByteBuffer] = ByteBuffer(4)
              lenBuf.putInt(group.length)
              lenBuf.unsafeImmutable.array
            }.toArray

          // We can't fuse flatMaps automatically yet, so manually fuse.
          val logicalEntries: Rep[ForgeArray[Tup2[String, ForgeArray[Byte]]]] =
            allKV.flatMap { t =>
              val (k, group) = unpack(t)
              val logicalKeys = group.indices.map { i => hashMatrixLogicalKey(k, i) }
              val logicalValues = group.map { row => serialize(row) }
              logicalKeys.zip(logicalValues) { (a,b) => pack((a,b)) }
            }.toArray

          val logicalKeys: Rep[ForgeArray[String]] = logicalEntries.map(_._1)
          val logicalValues: Rep[ForgeArray[ForgeArray[Byte]]] = logicalEntries.map(_._2)

          // We seem to be getting write bandwidth on the logicalKeys of ~15MB/sec, while the Google benchmarks at
          // https://github.com/google/leveldb claim writes should be ~45MB/sec. This could be due to hardware, JNI,
          // or the Java driver. Write bandwidth on the masterKeys, which are random and tiny, is abysmal.
          val ks = chunkRows.keys
          hash.putAll(ks, masterValues, ks.length)
          hash.putAll(logicalKeys, logicalValues, logicalKeys.length)
        })

        hash
      }

      infix ("reduce") (CurriedMethodSignature(List(List(("zero", T)), List(("func", MString ==> T)), List(("rfunc", (T,T) ==> T))), T), addTpePars = T) implements composite ${
        var acc = zero

        processFileChunks($self, line => func(line), { (a: Rep[ForgeArray[T]]) =>
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
