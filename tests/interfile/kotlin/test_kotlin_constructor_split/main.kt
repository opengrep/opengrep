fun main() {
    val taintedInput = source()
    val user = User(taintedInput)
    val result = user.getProfile()

    // Test field assignment taint flow
    val taintedInput2 = source()
    val fieldUser = FieldUser()
    fieldUser.name = taintedInput2
    val fieldResult = fieldUser.getProfile()

    // Test intermethod taint flow
    val intermethodObj = IntermethodClass()
    val intermethodResult = intermethodObj.sinkMethod()

    // Test chained method call: Constructor(tainted).method()
    // ruleid: kotlin_constructor_sqli
    sink(User(source()).getProfile())

    return
}
