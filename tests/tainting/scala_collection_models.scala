// Test: Scala collection models for taint propagation
import scala.collection.mutable

object CollectionModelsTest {

  def testListAppend(tainted: String): Unit = {
    val list = mutable.ListBuffer[String]()
    list.append(tainted)
    // ruleid: scala-collection-taint
    sink(list)
  }

  def testMapPut(tainted: String): Unit = {
    val map = mutable.Map[String, String]()
    map.put("key", tainted)
    val value = map.get("key")
    // ruleid: scala-collection-taint
    sink(value)
  }

  def testListHead(tainted: String): Unit = {
    val list = List(tainted)
    val value = list.head
    // ruleid: scala-collection-taint
    sink(value)
  }

  def testListLast(tainted: String): Unit = {
    val list = List(tainted)
    val value = list.last
    // ruleid: scala-collection-taint
    sink(value)
  }

  def sink(data: Any): Unit = {
    println(data)
  }
}
