// Cross-function field-sensitive taint through Scala's
// scala.collection.mutable.Map library-call recognisers. Maps flow
// across function boundaries as parameters / returns, and field-
// sensitivity must survive the call.

import scala.collection.mutable

object T {
  def writeBody(m: mutable.Map[String, String], v: String): Unit = {
    m.put("body", v)
  }

  def writeUser(m: mutable.Map[String, String], v: String): Unit = {
    m.put("user", v)
  }

  def readBody(m: mutable.Map[String, String]): Option[String] = {
    m.get("body")
  }

  def crossPos(): Unit = {
    val m = mutable.Map[String, String]()
    writeBody(m, source())
    // ruleid: test-library-access-taint
    sink(m.get("body"))
  }

  def crossNegSibling(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", "safe")
    writeUser(m, source())
    // ok: test-library-access-taint
    sink(m.get("body"))
  }

  def crossReadPos(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", source())
    // ruleid: test-library-access-taint
    sink(readBody(m))
  }
}
