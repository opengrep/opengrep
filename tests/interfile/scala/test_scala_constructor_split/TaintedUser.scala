class TaintedUser(seller: String) {
  private val key: String = source()

  def props(): Unit = {
    // ruleid: scala_constructor_sqli
    val query = sink(this.key)
    return
  }
}
