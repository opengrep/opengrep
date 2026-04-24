import scala.collection.mutable

object T {
  def handlerPutPos(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", source())
    m.put("user", "safe")
    // ruleid: taint
    sink(m.get("body"))
  }

  def handlerPutNeg(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", "safe")
    m.put("user", source())
    // ok: taint
    sink(m.get("body"))
  }

  def handlerUpdatePos(): Unit = {
    val m = mutable.Map[String, String]()
    m.update("body", source())
    // ruleid: taint
    sink(m.get("body"))
  }

  def handlerApplyAssignPos(): Unit = {
    val m = mutable.Map[String, String]()
    m("body") = source()
    m("user") = "safe"
    // ruleid: taint
    sink(m.get("body"))
  }

  def handlerApplyAssignNeg(): Unit = {
    val m = mutable.Map[String, String]()
    m("body") = "safe"
    m("user") = source()
    // ok: taint
    sink(m.get("body"))
  }

  def handlerApplyReadPos(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", source())
    // ruleid: taint
    sink(m("body"))
  }

  def handlerApplyReadNeg(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", "safe")
    m.put("user", source())
    // ok: taint
    sink(m("body"))
  }

  def handlerGetOrElsePos(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", source())
    // ruleid: taint
    sink(m.getOrElse("body", "fallback"))
  }

  def handlerGetOrElseTaintedDefault(): Unit = {
    val m = mutable.Map[String, String]()
    m.put("body", "safe")
    // ruleid: taint
    sink(m.getOrElse("body", source()))
  }

  def handlerPlusEqPos(): Unit = {
    val m = mutable.Map[String, String]()
    m += ("body" -> source())
    // ruleid: taint
    sink(m.get("body"))
  }

  def handlerPlusEqNeg(): Unit = {
    val m = mutable.Map[String, String]()
    m += ("body" -> "safe")
    m += ("user" -> source())
    // ok: taint
    sink(m.get("body"))
  }
}
