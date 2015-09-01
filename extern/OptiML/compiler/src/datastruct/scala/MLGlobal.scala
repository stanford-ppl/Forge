package optiml.compiler.datastruct.scala

import java.io.{File,BufferedReader,FileReader,PrintWriter,BufferedWriter,FileWriter}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._
import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import annotation.meta.beanGetter
import beans.BeanProperty
import com.amazonaws.services.dynamodbv2.datamodeling._
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
  private val nextId = new AtomicInteger(0)

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
        val id = nextId.getAndIncrement()
        val e = identifierDB.get(s.getBytes)
        if (e != null) {
          bytesToInt(e)
        }
        else {
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

  def loadUniqueMappings(path: String) = {
    val options = new Options()
    options.createIfMissing(true)
    // options.cacheSize(100000000L)
    identifierDB = factory.open(new java.io.File(path), options)
    reverseIdentifierDB = factory.open(new java.io.File(reverseIdentifierPath(path)), options)
  }

  def dumpUniqueMappings(path: String) = {
    identifierDB.close()
    identifierDB = null
    reverseIdentifierDB.close()
    reverseIdentifierDB = null
  }
}
