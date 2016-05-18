package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 *
 * A disk-backed key value store based on LevelDB
 * Keys are maintained in sorted order
 * Each key may reference a single value (get) or a collection of values (getAll)
 *
 */

trait KeyValueStoreOps {
  this: ForgeApplication =>

  /* we logically have a Key -> Coll[Value] store, implemented as a key := LogicalKey_Sep_UniqueId -> Value
   * This is done so that a logical append corresponds to only a physical put rather than a get-concat-put
   * LevelDB sorts data by key, so using the logical key as a prefix makes LevelDB keep all Coll[Value] adjacent on disk
   */

  def importKeyValueStoreOps() {
    val KV_LOGICAL_KEY_SEPARATOR = "_LK_"
    val Tup2 = lookupTpe("Tup2")
    val V = tpePar("V")
    val R = tpePar("R")
    val KeyValueStore = tpe("KeyValueStore", V)

    val LevelDB = tpe("org.iq80.leveldb.DB")
    primitiveTpePrefix ::= "org.iq80"

    data(KeyValueStore, ("_table", MString), ("_db", LevelDB), ("_deserialize", MLambda(Tup2(KeyValueStore(V),MString), V)))

    static (KeyValueStore) ("apply", V, (("table", MString), ("deserialize", (KeyValueStore(V),MString) ==> V)) :: KeyValueStore(V), effect = mutable) implements composite ${
      val hash = kv_alloc_raw[V](table, deserialize)
      hash.open()
      hash
    }

    compiler (KeyValueStore) ("kv_alloc_raw", V, (("table", MString), ("deserialize", (KeyValueStore(V),MString) ==> V)) :: KeyValueStore(V), effect = mutable) implements
      allocates(KeyValueStore, ${$0}, "unit(null.asInstanceOf[org.iq80.leveldb.DB])", ${doLambda((t: Rep[Tup2[KeyValueStore[V],String]]) => deserialize(t._1, t._2))})

    // -- code generated internal methods interface with the embedded db

    // We use simple effects in lieu of read / write effects because these are codegen nodes,
    // so we cannot pass the struct to them (a limitation of Forge at the moment).

    compiler (KeyValueStore) ("kv_open_internal", Nil, MString :: LevelDB, effect = simple) implements codegen($cala, ${
      import org.iq80.leveldb._
      import org.fusesource.leveldbjni.JniDBFactory._
      val options = new Options()
      options.createIfMissing(true)
      // options.cacheSize(100000000L)
      val db = factory.open(new java.io.File($0), options)
      db
    })

    compiler (KeyValueStore) ("kv_contains_internal", Nil, (LevelDB, MArray(MByte)) :: MBoolean, effect = simple) implements codegen($cala, ${
      val key = $1
      val iterator = $0.iterator()
      iterator.seek(key)
      val res = iterator.hasNext && {
        val foundKey = iterator.next.getKey
        java.util.Arrays.equals(key, foundKey) || foundKey.startsWith(key ++ "\$KV_LOGICAL_KEY_SEPARATOR".getBytes)
      }
      iterator.close()
      res
    })

    compiler (KeyValueStore) ("kv_get_internal", Nil, (LevelDB, MArray(MByte)) :: MArray(MByte), effect = simple) implements codegen($cala, ${
      $0.get($1)
    })

    compiler (KeyValueStore) ("kv_get_all_internal", Nil, (LevelDB, MString) :: MArray(MArray(MByte)), effect = simple) implements codegen($cala, ${
      // workaround for named arguments in codegen methods not working
      val db = $0
      val prefix = ($1 + "\$KV_LOGICAL_KEY_SEPARATOR").getBytes

      val buf = scala.collection.mutable.ArrayBuffer[Array[Byte]]()
      val iterator = db.iterator()
      iterator.seek(prefix) // first key that comes after prefix lexicographically

      var continue = true
      while (iterator.hasNext && continue) {
        val pair = iterator.next()
        val key = pair.getKey
        if (key.startsWith(prefix)) {
          buf += pair.getValue
        } else {
          continue = false
        }
      }

      iterator.close()
      if (buf.size > 0) buf.toArray else null
    })

    compiler (KeyValueStore) ("kv_put_internal", Nil, (LevelDB, MArray(MByte), MArray(MByte)) :: MUnit, effect = simple) implements codegen($cala, ${
      $0.put($1, $2)
    })

    compiler (KeyValueStore) ("kv_put_all_internal", Nil, (LevelDB, MArray(MString), MArray(MString), MArray(MArray(MByte)), MInt) :: MUnit, effect = simple) implements codegen($cala, ${
      assert($1.length >= $4 && $2.length >= $4 && $3.length >= $4, "KeyValueStore putAll called with too small arrays")
      val batch = $0.createWriteBatch()
      var i = 0
      while (i < $4) {
        val key = ($1(i) + "\$KV_LOGICAL_KEY_SEPARATOR" + $2(i)).getBytes
        batch.put(key, $3(i))
        i += 1
      }
      $0.write(batch)
      batch.close()
    })

    compiler (KeyValueStore) ("kv_close_internal", Nil, LevelDB :: MUnit, effect = simple) implements codegen($cala, ${
      $0.close()
    })

    compiler (KeyValueStore) ("kv_keys_internal", Nil, LevelDB :: MArray(MString)) implements codegen($cala, ${
      val buf = scala.collection.mutable.ArrayBuffer[String]()
      val iterator = $0.iterator()
      iterator.seekToFirst()
      var lastKey: String = null

      while (iterator.hasNext) {
        val dbKey = new String(iterator.next.getKey)
        if ((lastKey eq null) || !dbKey.startsWith(lastKey)) {
          val sepIndex = dbKey.indexOf("\$KV_LOGICAL_KEY_SEPARATOR")
          val key = if (sepIndex != -1) dbKey.substring(0, sepIndex) else dbKey
          lastKey = key + "\$KV_LOGICAL_KEY_SEPARATOR" // only skip logical keys
          buf += key
        }
      }

      iterator.close()
      buf.toArray
    })

    // --

    val KeyValueStoreOps = withTpe(KeyValueStore)
    KeyValueStoreOps {
      compiler ("kv_deserialize") (Nil :: MLambda(Tup2(KeyValueStore(V),MString), V)) implements getter(0, "_deserialize")
      compiler ("kv_table_name") (Nil :: MString) implements getter(0, "_table")
      compiler ("kv_get_db") (Nil :: LevelDB) implements getter(0, "_db")
      compiler ("kv_set_db") (LevelDB :: MUnit, effect = write(0)) implements setter(0, "_db", ${$1})

      compiler ("kv_get_db_safe") (Nil :: LevelDB) implements composite ${
        val db = kv_get_db($self)
        fassert(db != null, "No DB opened in KeyValueStore")
        db
      }

      infix ("open") (Nil :: MUnit, effect = write(0)) implements single ${
        val table = kv_table_name($self)
        val db = kv_open_internal(table)
        kv_set_db($self, db)
      }

      infix ("apply") (MString :: V) implements composite ${
        val lambda = kv_deserialize($self)
        doApply(lambda, pack(($self,$1)))
      }

      // This may be too inefficient, since a subsequent get has to hit the hash again.
      // However, if it's cached, it should be fine.
      infix ("contains") (MString :: MBoolean) implements single ${
        kv_contains_internal(kv_get_db_safe($self), $1.getBytes)
      }

      infix ("keys") (Nil :: MArray(MString)) implements single ${
        kv_keys_internal(kv_get_db_safe($self))
      }

      infix ("get") (MString :: MArray(MByte)) implements single ${
        kv_get_internal(kv_get_db_safe($self), $1.getBytes)
      }

      infix ("put") ((MString, MArray(MByte)) :: MUnit, effect = write(0)) implements single ${
        kv_put_internal(kv_get_db_safe($self), $1.getBytes, $2)
      }

      //get all values associated with the supplied logical key (prefix)
      infix ("getAll") (MString :: MArray(MArray(MByte))) implements single ${
        kv_get_all_internal(kv_get_db_safe($self), $1)
      }

      infix ("putAll") ((MArray(MString), MArray(MString), MArray(MArray(MByte)), MInt) :: MUnit, effect = write(0)) implements composite ${
        kv_put_all_internal(kv_get_db_safe($self), $1, $2, $3, $4)
      }

      infix ("close") (Nil :: MUnit, effect = write(0)) implements single ${
        kv_close_internal(kv_get_db_safe($self))
        kv_set_db($self, unit(null.asInstanceOf[org.iq80.leveldb.DB]))
      }
    }
  }
}
