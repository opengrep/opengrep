object Main {
  def main(args: Array[String]): Unit = {
    val taintedUser = new TaintedUser("seller")
    val props = taintedUser.props()
    val taintedInput = source()
    val user = new User(taintedInput)
    val result = user.getProfile()
    
    // Test field assignment taint flow
    val taintedInput2 = source()
    val fieldUser = new FieldUser()
    fieldUser.name = taintedInput2
    val fieldResult = fieldUser.getProfile()

    // Test chained method call: new Constructor(tainted).method()
    // ruleid: scala_constructor_sqli
    sink(new User(source()).getProfile())

    return
  }
}
