package optiml.library

import scala.collection.JavaConverters._

import java.io.{File,BufferedReader,FileReader,PrintWriter,BufferedWriter,FileWriter}
import java.util.HashMap

/*
 * Global values used by the interpreter.
 */

object MLGlobal {
  /* A single-threaded HashMap for mapping string identifiers to unique integer ids */
  private val identifierMap = new java.util.HashMap[String,Int]()
  private var nextId = 0

  def getId(s: String) = {
    if (identifierMap.containsKey(s)) {
      identifierMap.get(s)
    }
    else {
      identifierMap.put(s, nextId)
      nextId += 1
      nextId - 1
    }
  }

  // REFACTOR: Most of the below is duplicated from compiler/src/datastruct/scala/MLGlobal.scala,
  // but we don't have a good way to reuse data structure code between the compiler and interpreter
  // versions at the moment.

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
        if (id > nextId) nextId = id+1
        line = f.readLine()
      }
      f.close()
    }
    nextId-1
  }

  def dumpUniqueMappings(path: String) = {
    val f = new PrintWriter(new BufferedWriter(new FileWriter(path)))
    for (e <- identifierMap.entrySet().asScala) {
      f.println(e.getKey+MAPPING_DELIMITER+e.getValue)
    }
    f.close()
  }
}
