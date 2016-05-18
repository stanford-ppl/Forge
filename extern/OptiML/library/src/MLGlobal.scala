package optiml.library

import java.io.{File,BufferedReader,FileReader,PrintWriter,BufferedWriter,FileWriter}
import java.util.HashMap
import scala.collection.JavaConverters._

/*
 * Global values used by the interpreter.
 */

object MLGlobal {
  /* A single-threaded HashMap for mapping string identifiers to unique integer ids */
  private val identifierMap = new java.util.HashMap[String,Int]()
  private val reverseIdentifierMap = new java.util.HashMap[Int,String]()
  private var nextId = 0

  def getId(s: String) = {
    if (identifierMap.containsKey(s)) {
      identifierMap.get(s)
    }
    else {
      identifierMap.put(s, nextId)
      reverseIdentifierMap.put(nextId, s)
      nextId += 1
      nextId - 1
    }
  }

  def lookupId(i: Int) = {
    reverseIdentifierMap.get(i)
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

  def loadUniqueMappings(path: String) = {
    if (new File(path).isFile) {
      val f = new BufferedReader(new FileReader(path))
      var line = f.readLine()
      while (line != null) {
        val tokens = line.split(MAPPING_DELIMITER)
        val id = tokens(1).toInt
        identifierMap.put(tokens(0), id)
        reverseIdentifierMap.put(id, tokens(0))
        if (id > nextId) nextId = id+1
        line = f.readLine()
      }
      f.close()
    }
  }

  def dumpUniqueMappings(path: String) = {
    val f = new PrintWriter(new BufferedWriter(new FileWriter(path)))
    for (e <- identifierMap.entrySet().asScala) {
      f.println(e.getKey+MAPPING_DELIMITER+e.getValue)
    }
    f.close()
  }
}
