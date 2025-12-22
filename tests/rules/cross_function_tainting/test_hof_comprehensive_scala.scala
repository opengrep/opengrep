// Comprehensive HOF test for Scala: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

object TestHOF {
  // ===== Custom HOF Functions =====

  def customMap[T](arr: List[T], callback: T => T): List[T] = {
    var result = List[T]()
    for (item <- arr) {
      result = result :+ callback(item)
    }
    result
  }

  def customForEach[T](arr: List[T], callback: T => Unit): Unit = {
    for (item <- arr) {
      callback(item)
    }
  }

  def directCall[T](callback: T => Unit, value: T): Unit = {
    callback(value)
  }

  // ===== Test Cases =====

  def test_custom_map(): Unit = {
    val arr = List(source())
    customMap(arr, (x: String) => {
      // ruleid: test-hof-taint
      sink(x)
      x
    })
  }

  def test_custom_foreach(): Unit = {
    val arr = List(source())
    customForEach(arr, (x: String) => {
      // ruleid: test-hof-taint
      sink(x)
    })
  }

  def test_direct_call(): Unit = {
    directCall((x: String) => {
      // ruleid: test-hof-taint
      sink(x)
    }, source())
  }

  // ===== Built-in collection methods =====

  def test_builtin_map(): Unit = {
    val arr = List(source())
    arr.map(x => {
      // ruleid: test-hof-taint
      sink(x)
      x
    })
  }

  def test_builtin_foreach(): Unit = {
    val arr = List(source())
    arr.foreach(x => {
      // ruleid: test-hof-taint
      sink(x)
    })
  }

  def test_builtin_filter(): Unit = {
    val arr = List(source())
    arr.filter(x => {
      // ruleid: test-hof-taint
      sink(x)
      true
    })
  }

  // Stub functions
  def source(): String = "tainted"
  def sink(s: String): Unit = {}

  def main(args: Array[String]): Unit = {
    test_custom_map()
    test_custom_foreach()
    test_direct_call()
    test_builtin_map()
    test_builtin_foreach()
    test_builtin_filter()
  }
}
