package optiml.library

import scala.collection.JavaConverters._
import annotation.meta.beanGetter
import beans.BeanProperty
import com.amazonaws.services.dynamodbv2.datamodeling._
import java.nio.ByteBuffer
import java.io.{File,BufferedReader,FileReader,PrintWriter,BufferedWriter,FileWriter}
import java.util.HashMap

@DynamoDBTable(tableName="default")
case class KeyValue (
  // meta annotations create java-style getter/setter and place the dynamodb annotation on the getter
  @(DynamoDBHashKey @beanGetter) @BeanProperty var hashKey: String, // must be var for dynamo
  @(DynamoDBRangeKey @beanGetter) @BeanProperty var rangeKey: String,
  @(DynamoDBAttribute @beanGetter) @BeanProperty var value: ByteBuffer
) {
  def this() = this(null, null, null) // needed by dynamo to instantiate instances
  def this(hashKey: String) = this(hashKey, null, null) //convenient to perform queries by hash-key
}

/**
 * Forward Dynamo versions back to the in-memory version, in interpreter mode.
 */
object MLGlobalDynamo {

  def getId(s: String) = {
    MLGlobal.getId(s)
  }

  def lookupId(i: Int) = {
    MLGlobal.lookupId(i)
  }

  def getUniqueNames: Array[String] = {
    MLGlobal.getUniqueNames
  }

  def getUniqueIds: Array[Int] = {
    MLGlobal.getUniqueIds
  }

  def loadUniqueMappings(path: String) = {
    MLGlobal.loadUniqueMappings(path)
  }

  def dumpUniqueMappings(path: String) = {
    MLGlobal.dumpUniqueMappings(path)
  }
}
