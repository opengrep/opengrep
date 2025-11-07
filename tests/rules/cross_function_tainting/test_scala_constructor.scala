class TaintedUser(seller: String) {
  private val key: String = source()

  def props(): Unit = {
    // ruleid: scala_constructor_sqli
    val query = sink(this.key)
    return
  }
}

class User(userName: String) {
  private val name: String = userName
  
  def getProfile(): String = {
    // ruleid: scala_constructor_sqli
    val query = sink(this.name)
    return query
  }
}

class FieldUser {
  var name: String = ""
  
  def getProfile(): String = {
    // ruleid: scala_constructor_sqli
    val query = sink(this.name)
    return query
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val taintedInput = source()
    val user = new User(taintedInput)
    val result = user.getProfile()
    
    // Test field assignment taint flow
    val taintedInput2 = source()
    val fieldUser = new FieldUser()
    fieldUser.name = taintedInput2
    val fieldResult = fieldUser.getProfile()
    
    return
  }
}
