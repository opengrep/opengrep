class FieldUser {
  var name: String = ""
  
  def getProfile(): String = {
    // ruleid: scala_constructor_sqli
    val query = sink(this.name)
    return query
  }
}
