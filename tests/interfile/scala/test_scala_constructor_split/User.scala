class User(userName: String) {
  private val name: String = userName
  
  def getProfile(): String = {
    // ruleid: scala_constructor_sqli
    val query = sink(this.name)
    return query
  }
}
