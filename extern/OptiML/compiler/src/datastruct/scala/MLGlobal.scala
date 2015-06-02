package optiml.compiler.datastruct.scala

import java.io.{File,BufferedReader,FileReader,PrintWriter,BufferedWriter,FileWriter}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._
import ppl.delite.runtime.Config

/*
 * Global values used in generated OptiML code.
 */

object MLGlobal {
  /* A thread-safe HashMap for mapping string identifiers to unique integer ids */
  // TODO: For distributed execution, slaves should send a message back to the master to handle this.
  private val identifierMap = new ConcurrentHashMap[String,Int](16, 0.75f, Config.numThreads)
  private val nextId = new AtomicInteger(0)

  def getId(s: String) = {
    if (identifierMap.containsKey(s)) {
      identifierMap.get(s)
    }
    else {
      val id = nextId.getAndIncrement()
      // If someone else assigns an id to this string first, the next string will skip an id value.
      // While this is not ideal, it is still correct, as a contiguous id range is not guaranteed.
      val z = identifierMap.putIfAbsent(s, id)
      if (z == 0) id else z
    }
  }

  def getUniqueNames: Array[String] = {
    identifierMap.entrySet().asScala.map(e => e.getKey).toArray
  }

  def getUniqueIds: Array[Int] = {
    identifierMap.entrySet().asScala.map(e => e.getValue).toArray
  }

  // Something we don't expect to see in client data, but that is also human readable.
  val MAPPING_DELIMITER = " ::---> "

  def loadUniqueMappings(path: String): Int = {
    if (new File(path).isFile) {
      val f = new BufferedReader(new FileReader(path))
      var line = f.readLine()
      while (line != null) {
        val tokens = line.split(MAPPING_DELIMITER)
        val id = tokens(1).toInt
        identifierMap.put(tokens(0), id)
        if (id > nextId.get) nextId.set(id+1)
        line = f.readLine()
      }
      f.close()
    }
    nextId.get-1
  }

  def dumpUniqueMappings(path: String) = {
    val f = new PrintWriter(new BufferedWriter(new FileWriter(path)))
    for (e <- identifierMap.entrySet().asScala) {
      f.println(e.getKey+MAPPING_DELIMITER+e.getValue)
    }
    f.close()
  }
}
