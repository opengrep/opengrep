package constructor_taint

class Store(val data: String) {
  def send(): Unit = {
    // ruleid: test-constructor-taint
    Store.sink(this.data)
  }
}

object Store {
  def sink(x: String): Unit = {
    println(x)
  }
}
