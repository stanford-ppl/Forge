package optiml.compiler.datastruct.scala

import java.io.{File,BufferedReader,FileReader,PrintWriter,BufferedWriter,FileWriter}
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.document._
import com.amazonaws.services.dynamodbv2.model._
import com.amazonaws.services.dynamodbv2.document.spec._

import com.amazonaws.services.dynamodbv2.datamodeling._
import DynamoDBMapperConfig.ConsistentReads._
import DynamoDBMapperConfig.TableNameOverride._
import annotation.meta.beanGetter
import beans.BeanProperty

import ppl.delite.runtime.Config

/**
 * This class is used in DHashStream.
 */
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
 * This is used in Feature.scala for DynamoDB-backed versions of 'unique'.
 *
 * Caution: This unique map implementation has not been vetted for performance, and initial experiments
 * suggest that it requires extremely high throughput provisions to not become a bottleneck. It should be
 * considered experimental and used only with care.
 */
object MLGlobalDynamo {
  private var client: DynamoDB = null // intentionally null until loaded
  private var identifierDB: Table = null // intentionally null until loaded
  private var reverseIdentifierDB: Table = null // intentionally null until loaded

  // The global counter is stored inside the DynamoDB "identifierDB" table under the special key COUNTER_KEY
  // COUNTER_KEY has one attribute NEXT_ID, whose value is an int containing the next id.
  val COUNTER_KEY = "____CK"
  val NEXT_ID = "NextId"

  // These are needed to atomically increment the global counter
  val incrementValueMap = new java.util.HashMap[String,Object]()
  incrementValueMap.put(":n", new Integer(1))
  val increment = "set " + NEXT_ID + " = " + NEXT_ID + " + :n"

  // Every key in identifierDB / reverseIdentifierDB stores one attribute, ATTR_ID, containing its id
  val ATTR_ID = "id"

  def getId(s: String): Int = {
    assert(identifierDB != null, "error: unique DB is not loaded")
    assert(reverseIdentifierDB != null, "error: unique DB is not loaded")

    // We can't insert empty strings, so we reserve id 0 for that
    if (s == "") 0
    else {
      try {
        val key = new PrimaryKey("hashKey", s)
        val get = new GetItemSpec().withPrimaryKey(key).withConsistentRead(true)
        val e = identifierDB.getItem(get)
        if (e != null) {
          e.getInt(ATTR_ID)
        }
        else {
          // atomically update COUNTER_KEY, returning the new value
          val c = new PrimaryKey("hashKey", COUNTER_KEY)
          val update = new UpdateItemSpec().withPrimaryKey(c).withUpdateExpression(increment).withValueMap(incrementValueMap).withReturnValues(ReturnValue.UPDATED_NEW)
          val res = identifierDB.updateItem(update)
          val id = res.getItem.getInt(NEXT_ID)

          // Now we can safely store the returned id as the id for this string
          val put = new PutItemSpec().withItem(new Item().withPrimaryKey(key).withInt(ATTR_ID, id))
                                     .withExpected(new Expected(ATTR_ID).notExist())

          val reverseKey = new PrimaryKey("hashKey", id.toString)
          val putReverse = new PutItemSpec().withItem(new Item().withPrimaryKey(reverseKey).withString(ATTR_ID, s))
                                            .withExpected(new Expected(ATTR_ID).notExist())
          try {
            identifierDB.putItem(put)
            reverseIdentifierDB.putItem(putReverse)
            id
          }
          catch {
            case e:ConditionalCheckFailedException =>
              // someone got here first, get the new value
              val e = identifierDB.getItem(get)
              assert(e != null, "dUnique: expected DynamoDB item for to exist, but failed to get")
              e.getInt(ATTR_ID)
          }
        }
      }
      catch {
        case e: Exception =>
          println("dUnique: failed to insert unique id for " + s + " into DynamoDB due to exception: " + e + " (returning -1)")
          -1
      }
    }
  }

  def lookupId(id: Int): String = {
    assert(reverseIdentifierDB != null, "error: unique DB is not loaded")

    if (id == 0) ""
    else {
      val key = new PrimaryKey("hashKey", id.toString)
      val get = new GetItemSpec().withPrimaryKey(key).withConsistentRead(true)
      try {
        val e = reverseIdentifierDB.getItem(get)
        if (e == null) null else { e.getString(ATTR_ID) }
      }
      catch {
        case e: Exception =>
          println("dUnique: failed to lookup unique name for " + id + " from DynamoDB due to exception: " + e + " (returning null)")
          null
      }
    }
  }

  def getUniqueIds: Array[Int] = {
    assert(reverseIdentifierDB != null, "error: unique DB is not loaded")

    // Here, we look up all the values (ints) associated with string ids
    val buf = new scala.collection.mutable.HashSet[Int]
    val scan = new ScanSpec().withProjectionExpression(ATTR_ID)

    val results = identifierDB.scan(scan)
    val iterator = results.iterator

    while (iterator.hasNext) {
      val i = iterator.next
      buf += i.getInt(ATTR_ID)
    }

    buf.toArray
  }

  def getUniqueNames: Array[String] = {
    assert(identifierDB != null, "error: unique DB is not loaded")

    // Here, we look up all the values (strings) associated with int ids
    val buf = new scala.collection.mutable.HashSet[String]
    val scan = new ScanSpec().withProjectionExpression(ATTR_ID)

    val results = reverseIdentifierDB.scan(scan)
    val iterator = results.iterator

    while (iterator.hasNext) {
      val i = iterator.next
      buf += i.getString(ATTR_ID)
    }

    buf.toArray
  }

  private def reverseIdentifierPath(path: String) = { path + "-reverse" }

  def loadUniqueMappings(path: String) = {
    val lowLevelClient = new AmazonDynamoDBClient()
    val regionName = sys.env.getOrElse("AWS_DEFAULT_REGION", "us-west-2")
    lowLevelClient.configureRegion(com.amazonaws.regions.Regions.fromName(regionName))
    client = new DynamoDB(lowLevelClient)
    // TODO: need to create table if it doesn't exist, configure throughput, etc.
    //       Also need to insert COUNTER_KEY into the table with a value of 1 when first created.
    identifierDB = client.getTable(path)
    reverseIdentifierDB = client.getTable(reverseIdentifierPath(path))
  }

  def dumpUniqueMappings(path: String) = {
    identifierDB = null
    reverseIdentifierDB = null
    client.shutdown()
    client = null
  }
}
