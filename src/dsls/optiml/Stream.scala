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
    importDHashStreamOps()
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

  def importDHashStreamOps() {
    val DHashStream = lookupTpe("DHashStream")
    val FileStream = lookupTpe("FileStream")
    val DenseVector = lookupTpe("DenseVector")
    val ByteBuffer = tpe("java.nio.ByteBuffer")
    val Tup2 = lookupTpe("Tup2")
    val V = tpePar("V")
    val R = tpePar("R")

    // Unfortunately, we need to do a bit of our own application-level parallelism to
    // enable reading a file and writing to DynamoDB asynchronously, which is required to
    // maximize throughput.
    importConcurrentQueue()
    importThreads()
    val CQueue = lookupTpe("java.util.concurrent.ArrayBlockingQueue")
    val SThread = lookupTpe("java.lang.Thread")

    val DB = tpe("com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper")
    primitiveTpePrefix ::= "com.amazonaws"
    val SecretKey = tpe("javax.crypto.spec.SecretKeySpec")
    primitiveTpePrefix ::= "javax.crypto"

    data(DHashStream, ("_table", MString), ("_key", SecretKey), ("_db", DB), ("_deserialize", MLambda(Tup2(DHashStream(V),MString), V)))

    static (DHashStream) ("apply", V, (("table", MString), ("deserialize", (DHashStream(V),MString) ==> V)) :: DHashStream(V), effect = mutable) implements composite ${
      val hash = dhash_alloc_raw[V](table, deserialize)
      hash.open()
      hash
    }

    compiler (DHashStream) ("dhash_alloc_raw", V, (("table", MString), ("deserialize", (DHashStream(V),MString) ==> V)) :: DHashStream(V), effect = mutable) implements
      allocates(DHashStream, ${$0}, "unit(null.asInstanceOf[javax.crypto.spec.SecretKeySpec])", "unit(null.asInstanceOf[com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper])", ${doLambda((t: Rep[Tup2[DHashStream[V],String]]) => deserialize(t._1, t._2))})

    // -- code generated internal methods interface with the embedded db

    // We use simple effects in lieu of read / write effects because these are codegen nodes,
    // so we cannot pass the struct to them (a limitation of Forge at the moment).

    compiler (DHashStream) ("dhash_create_table_internal", Nil, (MString, MInt, MInt) :: MUnit, effect = simple) implements codegen($cala, ${
      import com.amazonaws.services.dynamodbv2._
      import com.amazonaws.services.dynamodbv2.util.Tables
      import com.amazonaws.services.dynamodbv2.model._
      import com.amazonaws.services.dynamodbv2.document._

      try {
        val client = new AmazonDynamoDBClient()
        val regionName = sys.env.getOrElse("AWS_DYNAMO_REGION", sys.env.getOrElse("AWS_DEFAULT_REGION", "us-east-1"))
        client.configureRegion(com.amazonaws.regions.Regions.fromName(regionName))

        if (Tables.doesTableExist(client, $0)) {
          println("[optiml stream]: deleting existing table " + $0)
          val docClient = new DynamoDB(client)
          val t = docClient.getTable($0)
          t.delete()
          t.waitForDelete()
        }

        // This corresponds to the schema in MLGlobalDynamo.scala.
        val hashKey = new KeySchemaElement().withAttributeName("hashKey").withKeyType(KeyType.HASH)
        val rangeKey = new KeySchemaElement().withAttributeName("rangeKey").withKeyType(KeyType.RANGE)
        val throughput = new ProvisionedThroughput($1, $2)
        val create = new CreateTableRequest()
          .withTableName($0)
          .withKeySchema(hashKey, rangeKey)
          .withAttributeDefinitions(new AttributeDefinition("hashKey", ScalarAttributeType.S),
                                    new AttributeDefinition("rangeKey", ScalarAttributeType.S))
          .withProvisionedThroughput(throughput)

        client.createTable(create)
        println("[optiml stream]: created DynamoDB table " + $0 + " - waiting to become active")
        Tables.awaitTableToBecomeActive(client, $0)
        client.shutdown()
      }
      catch {
        case e: Throwable =>
          println("[optiml stream]: fatal error: failed to create table " + $0)
          throw e
      }
    })

    compiler (DHashStream) ("dhash_open_internal", Nil, MString :: DB, effect = simple) implements codegen($cala, ${
      import com.amazonaws.services.dynamodbv2._
      import com.amazonaws.services.dynamodbv2.datamodeling._

      // TODO: Configure client for tuning options.
      // see: http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/ClientConfiguration.html
      val dynamoClient = new AmazonDynamoDBClient()
      val regionName = sys.env.getOrElse("AWS_DYNAMO_REGION", sys.env.getOrElse("AWS_DEFAULT_REGION", "us-east-1"))
      dynamoClient.configureRegion(com.amazonaws.regions.Regions.fromName(regionName))
      val config = new DynamoDBMapperConfig.Builder().withTableNameOverride(DynamoDBMapperConfig.TableNameOverride.withTableNameReplacement($0)).withConsistentReads(DynamoDBMapperConfig.ConsistentReads.EVENTUAL).build()
      new DynamoDBMapper(dynamoClient, config)
    })

    compiler (DHashStream) ("dhash_lookup_key_internal", Nil, Nil :: SecretKey, effect = simple) implements codegen($cala, ${
      import com.amazonaws.services.kms._

      val kmsClient = new AWSKMSClient()
      val kmsRegion = sys.env.getOrElse("AWS_KMS_REGION", sys.env.getOrElse("AWS_DEFAULT_REGION", "us-east-1"))
      kmsClient.configureRegion(com.amazonaws.regions.Regions.fromName(kmsRegion))

      val s3Client = new com.amazonaws.services.s3.AmazonS3Client()
      val s3Region = sys.env.getOrElse("AWS_S3_REGION", sys.env.getOrElse("AWS_DEFAULT_REGION", "us-east-1"))
      s3Client.configureRegion(com.amazonaws.regions.Regions.fromName(s3Region))

      val keyPath = sys.env.getOrElse("AWS_KMS_KEY_PATH", throw new RuntimeException("DHashStream: AWS_KMS_KEY_PATH is undefined"))
      val bucketName = keyPath.takeWhile(_ != '/')
      val keyName = keyPath.drop(bucketName.length + 1)

      val inputStream = s3Client.getObject(bucketName, keyName).getObjectContent
      val encryptedKey = new Array[Byte](256)
      val encryptedSize = inputStream.read(encryptedKey)
      assert(inputStream.read() == -1, "ERROR: S3 key contents larger than expected")
      inputStream.close()

      val decryptedKey = kmsClient.decrypt(new model.DecryptRequest().withCiphertextBlob(java.nio.ByteBuffer.wrap(encryptedKey, 0, encryptedSize))).getPlaintext.array
      new javax.crypto.spec.SecretKeySpec(decryptedKey, "AES")
    })

    compiler (DHashStream) ("dhash_contains_internal", Nil, (DB, MString, MString) :: MBoolean, effect = simple) implements codegen($cala, ${
      try {
        // First version throws NPEs
        // val query = (new com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression).withHashKeyValues(new KeyValue($1)).withSelect("COUNT").withLimit(1)
        // $0.queryPage(classOf[KeyValue], query).getCount > 0
        val query = (new com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression).withHashKeyValues(new KeyValue($1)).withLimit(1)
        // TODO: investigate - debug log output claims that this is triggering consistentReads, but it shouldn't.
        $0.count(classOf[KeyValue], query) > 0
      }
      catch {
        case p: com.amazonaws.services.dynamodbv2.model.ProvisionedThroughputExceededException =>
          // We could increase the provision.. doing nothing for now
          p.printStackTrace
          println("[optiml stream]: failed to lookup key " + $1 + " from DynamoDB from " + $2 + " (returning false).")
          false
        case e: Throwable =>
          println("[optiml stream]: caught unknown exception: " + e + " with " + $2 + " (returning false).")
          false
      }
    })

    compiler (DHashStream) ("dhash_get_internal", Nil, (DB, MString, MString) :: ByteBuffer, effect = simple) implements codegen($cala, ${
      $0.load(classOf[KeyValue], $1, $2).value
    })

    compiler (DHashStream) ("dhash_get_all_internal", Nil, (DB, SecretKey, MString, MString) :: MArray(ByteBuffer), effect = simple) implements codegen($cala, ${
      import javax.crypto._
      val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
      val ivLength = 16

      try {
        val list = $0.query(classOf[KeyValue], (new com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression).withHashKeyValues(new KeyValue($2)))
        if ((list == null) || list.isEmpty) {
          null
        }
        else {
          val res = new Array[java.nio.ByteBuffer](list.size)
          var i = 0
          var iter = list.iterator
          while (i < list.size) {
            val encrypted = iter.next.value

            val iv = new Array[Byte](ivLength)
            encrypted.get(iv)
            val cipherText = new Array[Byte](encrypted.remaining)
            encrypted.get(cipherText)
            cipher.init(Cipher.DECRYPT_MODE, $1, new spec.IvParameterSpec(iv))
            val decrypted = cipher.doFinal(cipherText)

            val gzip = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(decrypted))
            val decompressed = new java.io.ByteArrayOutputStream
            val chunkSize = 1024
            val buffer = new Array[Byte](chunkSize)
            var length = 0
            while (length != -1) {
              decompressed.write(buffer, 0, length)
              length = gzip.read(buffer, 0, chunkSize)
            }
            res(i) = java.nio.ByteBuffer.wrap(decompressed.toByteArray)
            i += 1
          }
          res
        }
      }
      catch {
        case p: com.amazonaws.services.dynamodbv2.model.ProvisionedThroughputExceededException =>
          // Should application-level retry?
          p.printStackTrace
          println("[optiml stream]: failed to read key " + $2 + " from DynamoDB from " + $3 + " (returning null).")
          null
        case e: Throwable =>
          println("[optiml stream]: caught unknown exception: " + e + " with " + $3 + " (returning null).")
          null
      }
    })

    compiler (DHashStream) ("dhash_put_internal", Nil, (DB, MString, MString, ByteBuffer) :: MUnit, effect = simple) implements codegen($cala, ${
      $0.save(new KeyValue($1, $2, $3))
    })

    // We use a background thread to constantly push writes to DynamoDB from a queue that is filled by readers,
    // as they process each file chunk. The input queue has a bounded size, so if readers race too far ahead
    // of writers, the readers will block, preventing unbounded memory growth.
    compiler (DHashStream) ("dhash_launch_background_writer_internal", Nil, (DB, CQueue(MAny)) :: SThread, effect = simple) implements codegen($cala, ${
      val numThreads: Int = System.getProperty("optiml.stream.dynamodb.threads","128").toInt
      val batchSize: Int = System.getProperty("optiml.stream.dynamodb.batchsize", "1000").toInt
      val inputQueue = $1.asInstanceOf[java.util.concurrent.ArrayBlockingQueue[KeyValue]]
      val workQueueSize = 100
      val workQueue = new java.util.concurrent.ArrayBlockingQueue[Runnable](workQueueSize)
      val pool = new java.util.concurrent.ThreadPoolExecutor(numThreads, numThreads, workQueueSize, java.util.concurrent.TimeUnit.SECONDS, workQueue)

      // This thread will run forever until the process dies or it is interrupted!
      val t = new Thread(new Runnable {
        // Keep track of the current batch, so we can drain if interrupted.
        var batchInProgress: java.util.ArrayList[KeyValue] = null

        def submitOne(batch: java.util.ArrayList[KeyValue]) = new Runnable {
          def run() = {
            val failures = $0.batchSave(batch)
            if (!failures.isEmpty) {
              failures.get(0).getException.printStackTrace()
            }
          }
        }

        def block() { Thread.sleep(100) }

        def drain() {
          val batch = new java.util.ArrayList[KeyValue]()
          if (batchInProgress != null) batch.addAll(batchInProgress)
          inputQueue.drainTo(batch)
          if (batch.size > 0) {
            while (workQueue.size > workQueueSize-1) block()
            pool.execute(submitOne(batch))
          }
          pool.shutdown()
          while (!pool.isTerminated()) block()
        }

        def run(): Unit = {
          while (true) {
            try {
              // Block if our pool is busy. This is necessary because the thread pool
              // will reject tasks if the queue is full and no threads are available,
              // rather than block.
              while (workQueue.size > workQueueSize-1) block()

              // Fill the next DynamoDB batch request (block if no data is ready)
              val batch = new java.util.ArrayList[KeyValue]()
              batchInProgress = batch
              while (batch.size < batchSize) {
                // Will block until the queue is ready
                val e = inputQueue.take()
                batch.add(e)
              }

              // Launch the request in an independent thread
              try {
                pool.execute(submitOne(batch))
                batchInProgress = null
              }
              catch {
                case e: java.util.concurrent.RejectedExecutionException => e.printStackTrace()
              }
            }
            catch {
              case e:InterruptedException => drain; return
            }
          }
        }
      })

      t.start()
      t
    })

    compiler (DHashStream) ("dhash_put_all_internal", Nil, MethodSignature(List(SecretKey, CQueue(MAny), MArray(MString), MArray(MString), MArray(MArray(MByte)), MInt), MUnit), effect = simple) implements codegen($cala, ${
      import javax.crypto._

      val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")

      assert($2.length >= $5, "DHashStream putAll called with too small arrays")
      val queue = $1.asInstanceOf[java.util.concurrent.ArrayBlockingQueue[KeyValue]]
      var i = 0
      while (i < $5) {
        //compress values before sending
        val compressed = new java.io.ByteArrayOutputStream
        val gzip = new java.util.zip.GZIPOutputStream(compressed)
        gzip.write($4(i))
        gzip.finish()

        cipher.init(Cipher.ENCRYPT_MODE, $0)
        val iv = cipher.getParameters.getParameterSpec(classOf[spec.IvParameterSpec]).getIV
        val encrypted = cipher.doFinal(compressed.toByteArray)

        val kv = new KeyValue($2(i), $3(i), java.nio.ByteBuffer.wrap(iv ++ encrypted))
        // Will block if the queue is already full
        queue.put(kv)
        i += 1
      }
      ()
    })

    compiler (DHashStream) ("dhash_keys_internal", Nil, DB :: MArray(MString)) implements codegen($cala, ${
      val buf = new scala.collection.mutable.HashSet[String]

      val scan = (new com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBScanExpression).withProjectionExpression("hashKey")
      val result = $0.parallelScan(classOf[KeyValue], scan, 16)
      val iter = result.iterator
      while (iter.hasNext) {
        buf += iter.next.hashKey
      }
      buf.toArray
    })


    // --

    val DHashStreamOps = withTpe(DHashStream)
    DHashStreamOps {
      compiler ("dhash_deserialize") (Nil :: MLambda(Tup2(DHashStream(V),MString), V)) implements getter(0, "_deserialize")
      compiler ("dhash_table_name") (Nil :: MString) implements getter(0, "_table")
      compiler ("dhash_get_db") (Nil :: DB) implements getter(0, "_db")
      compiler ("dhash_set_db") (DB :: MUnit, effect = write(0)) implements setter(0, "_db", ${$1})
      compiler ("dhash_get_key") (Nil :: SecretKey) implements getter(0, "_key")
      compiler ("dhash_set_key") (SecretKey :: MUnit, effect = write(0)) implements setter(0, "_key", ${$1})

      compiler ("dhash_get_db_safe") (Nil :: DB) implements composite ${
        val db = dhash_get_db($self)
        fassert(db != null, "No DB opened in DHashStream")
        db
      }

      infix ("open") (Nil :: MUnit, effect = write(0)) implements single ${
        val table = dhash_table_name($self)
        val db = dhash_open_internal(table)
        dhash_set_db($self, db)
        val key = dhash_lookup_key_internal()
        dhash_set_key($self, key)
      }

      infix ("apply") (MString :: V) implements composite ${
        val lambda = dhash_deserialize($self)
        doApply(lambda, pack(($self,$1)))
      }

      //TODO: can we reconcile the type signatures for the different DHashStream apis
      //e.g., need to pick either Array[Byte] or ByteBuffer, etc.

      // This may be too inefficient, since a subsequent get has to hit the hash again.
      // However, if it's cached, it should be fine.
      infix ("contains") (MString :: MBoolean) implements single ${
        dhash_contains_internal(dhash_get_db_safe($self), $1, dhash_table_name($self))
      }

      infix ("keys") (Nil :: MArray(MString)) implements single ${
        dhash_keys_internal(dhash_get_db_safe($self))
      }

      infix ("get") ((MString, MString) :: ByteBuffer) implements single ${
        dhash_get_internal(dhash_get_db_safe($self), $1, $2)
      }

      infix ("put") ((MString, MString, ByteBuffer) :: MUnit, effect = write(0)) implements single ${
        dhash_put_internal(dhash_get_db_safe($self), $1, $2, $3)
      }

      //get all values associated with the supplied logical key (prefix)
      infix ("getAll") (MString :: MArray(ByteBuffer)) implements single ${
        dhash_get_all_internal(dhash_get_db_safe($self), dhash_get_key($self), $1, dhash_table_name($self))
      }

      infix ("putAll") ((CQueue(MAny), MArray(MString), MArray(MString), MArray(MArray(MByte)), MInt) :: MUnit, effect = write(0)) implements composite ${
        dhash_put_all_internal(dhash_get_key($self), $1, $2, $3, $4, $5)
      }

      infix ("close") (Nil :: MUnit, effect = write(0)) implements single ${
        dhash_set_db($self, unit(null.asInstanceOf[com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper]))
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
        val lambda = dhash_deserialize($self)
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
    val DHashStream = lookupTpe("DHashStream")
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

    direct (FileStream) ("hashMatrixDeserializerD", Nil, (("hash", DHashStream(DenseMatrix(MDouble))), ("k", MString)) :: DenseMatrix(MDouble)) implements composite ${
      val rows = hash.getAll(k)
      if (rows == null) {
        unit(null).AsInstanceOf[DenseMatrix[Double]]
      }
      else {
        val numRows = rows.length
        val rowBuffer = rows(0)
        val numCols0 = rowBuffer.getInt()
        val out = DenseMatrix[Double](numRows, numCols0)
        val dst = densematrix_raw_data(out).unsafeMutable // array_update gets rewritten, but not the write below
        rowBuffer.unsafeImmutable.get(dst, 0, numCols0) // write directly to underlying matrix

        var i = 1
        while (i < numRows) {
          val rowBuffer = rows(i)
          val numCols = rowBuffer.getInt()
          fassert(numCols0 == numCols, "hashMatrixDeserializer: expected " + numCols0 + " cols, but found " + numCols)
          rowBuffer.unsafeImmutable.get(dst, i*numCols, numCols)
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
          ("outDelim", MString, "unit(\"    \")") // output delimiter is an ordinary string
        ),
        List(
          ("func", DenseVector(MString) ==> DenseVector(R))
        )), FileStream), TStringable(R), addTpePars = R) implements composite ${

        $self.map(outFile, preserveOrder = true) { line =>
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

        $self.processFileChunks({ (line, location) =>
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(delim, -1) // we preserve trailing empty values
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

      /**
       * Returns a DHashStream (backed by DynamoDB). If createTable = true, the table will be
       * deleted if it already exists. Use with caution!
       *
       * FIXME: refactor (unfortunate duplication for HashStream variants)
       */
      infix ("groupRowsByD") (CurriedMethodSignature(List(
        List(
          ("outTable", MString),
          ("delim", MString, "unit(\"\\s+\")"),   // input delimiter is a regular expression
          ("createTable", MBoolean, "unit(false)"),
          ("provision", Tup2(MInt,MInt), "tup2_pack((unit(1),unit(1)))")
        ),
        List(
          ("keyFunc", DenseVector(MString) ==> MString),
          ("valFunc", DenseVector(MString) ==> DenseVector(MDouble))
        )), DHashStream(DenseMatrix(MDouble)))) implements composite ${

        def serialize(v: Rep[DenseVector[Double]]): Rep[ForgeArray[Byte]] = {
          val x = ByteBuffer(4+8*v.length)
          x.putInt(v.length)
          x.put(v.toArray, 0, v.length)
          x.unsafeImmutable.array
        }

        val hash = DHashStream[DenseMatrix[Double]](outTable, hashMatrixDeserializerD)
        if (createTable) {
          dhash_create_table_internal(outTable, provision._1, provision._2)
        }

        // Launch background writer thread with 10M capacity queue
        val queueSize: Int = System.getProperty("optiml.stream.dynamodb.queuesize", "10000000").toInt
        val queue = CQueue[Any](queueSize)
        val t = dhash_launch_background_writer_internal(dhash_get_db_safe(hash), queue)

        $self.processFileChunks({ (line, location) =>
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(delim, -1) // we preserve trailing empty values
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          val key: Rep[String] = keyFunc(tokenVector)
          val value: Rep[DenseVector[Double]] = valFunc(tokenVector)
          pack((key, value, location))
        },
        { (a: Rep[ForgeArray[Tup3[String,DenseVector[Double],String]]]) =>

          val chunk = densevector_fromarray(a, true).filter(t => t._2.length > 0)
          val dbKeyPrefix: Rep[ForgeArray[String]] = chunk.map(t => t._1).toArray
          val dbKeySuffix: Rep[ForgeArray[String]] = chunk.map(t => t._3).toArray
          val dbValues: Rep[ForgeArray[ForgeArray[Byte]]] = chunk.map(t => serialize(t._2)).toArray

          hash.putAll(queue, dbKeyPrefix, dbKeySuffix, dbValues, dbKeyPrefix.length)
        })

        t.interrupt()
        t.join()
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
