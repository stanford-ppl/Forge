package optiml.compiler.datastruct.scala

import java.io.{File,BufferedReader,FileReader,PrintWriter,BufferedWriter,FileWriter}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._

import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import java.nio.ByteBuffer
import ppl.delite.runtime.Config

/*
 * Global values used in generated OptiML code.
 */

object MLGlobal {
  /*
   * In Delite mode, we use a thread-safe LevelDB instance to store the mapping of
   * string identifiers to unique integer ids.
   *
   * TODO: For distributed execution, slaves should send a message back to the master to handle this.
   */
  private object lock
  private var identifierDB: DB = null // intentionally null until loaded
  private var reverseIdentifierDB: DB = null // intentionally null until loaded
  private var nextId: AtomicInteger = null // intentionally null until loaded

  def intToBytes(i: Int) = java.nio.ByteBuffer.allocate(4).putInt(i).array
  def bytesToInt(a: Array[Byte]) = java.nio.ByteBuffer.wrap(a).getInt()

  def getId(s: String) = {
    assert(identifierDB != null, "error: unique DB is not loaded")
    assert(reverseIdentifierDB != null, "error: unique DB is not loaded")

    val e = identifierDB.get(s.getBytes)
    if (e != null) {
      bytesToInt(e)
    }
    else {
      lock.synchronized {
        val e = identifierDB.get(s.getBytes)
        if (e != null) {
          bytesToInt(e)
        }
        else {
          val id = nextId.getAndIncrement()
          val (k, v) = (s.getBytes, intToBytes(id))
          identifierDB.put(k, v)
          reverseIdentifierDB.put(v, k)
          id
        }
      }
    }
  }

  def lookupId(i: Int) = {
    assert(reverseIdentifierDB != null, "error: unique DB is not loaded")
    new String(reverseIdentifierDB.get(intToBytes(i)))
  }

  def getUniqueIds: Array[Int] = {
    assert(reverseIdentifierDB != null, "error: unique DB is not loaded")

    val buf = scala.collection.mutable.ArrayBuffer[Int]()
    val iterator = reverseIdentifierDB.iterator()
    iterator.seekToFirst()

    while (iterator.hasNext) {
      buf += bytesToInt(iterator.next.getKey)
    }

    iterator.close()
    buf.toArray
  }

  def getUniqueNames: Array[String] = {
    assert(identifierDB != null, "error: unique DB is not loaded")

    val buf = scala.collection.mutable.ArrayBuffer[String]()
    val iterator = identifierDB.iterator()
    iterator.seekToFirst()

    while (iterator.hasNext) {
      buf += new String(iterator.next.getKey)
    }

    iterator.close()
    buf.toArray
  }

  private def reverseIdentifierPath(path: String) = { path + "_reverse" }

  /**
   * We use a custom comparator for the reverseIdentifierDB map, which stores integer
   * ids as keys. The default comparator stores keys in lexicographic order, whereas we
   * want the natural (integer) ordering. This allows us to quickly retrieve the last id
   * stored in the DB, which is necessary to initialize our state.
   */
  private val intComparator = new DBComparator() {
    def compare(key1: Array[Byte], key2: Array[Byte]): Int ={
      bytesToInt(key1).compareTo(bytesToInt(key2))
    }

    def name(): String = "IntComparator"
    def findShortestSeparator(start: Array[Byte], limit: Array[Byte]) = start
    def findShortSuccessor(key: Array[Byte]) = key
  }

  def loadUniqueMappings(path: String) = {
    val idOptions = new Options()
    idOptions.createIfMissing(true)
    // idOptions.cacheSize(100000000L)
    identifierDB = factory.open(new java.io.File(path), idOptions)

    val reverseIdOptions = new Options()
    reverseIdOptions.createIfMissing(true)
    reverseIdOptions.comparator(intComparator)
    reverseIdentifierDB = factory.open(new java.io.File(reverseIdentifierPath(path)), reverseIdOptions)

    val i = reverseIdentifierDB.iterator()
    i.seekToLast()
    val lastId = if (i.hasNext) bytesToInt(i.next.getKey) else -1
    nextId = new AtomicInteger(lastId + 1)
    i.close()
  }

  def dumpUniqueMappings(path: String) = {
    identifierDB.close()
    identifierDB = null
    reverseIdentifierDB.close()
    reverseIdentifierDB = null
  }
}
