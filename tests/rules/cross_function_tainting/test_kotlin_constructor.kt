class TaintedUser {
    private var key: String = ""
    
    constructor() {
        this.key = source()
    }

    fun props() {
        // ruleid: kotlin_constructor_sqli
        val query = sink(this.key)
        return
    }
}

class User {
    private var name: String = ""
    
    constructor(userName: String) {
        this.name = userName
    }
    
    fun getProfile(): String {
        // ruleid: kotlin_constructor_sqli
        val query = sink(this.name)
        return query
    }
}

class FieldUser {
    var name: String = ""

    constructor() {
        this.name = ""
    }

    fun getProfile(): String {
        // ruleid: kotlin_constructor_sqli
        val query = sink(this.name)
        return query
    }
}

class IntermethodClass {
    fun taintMethod(): String {
        return source()
    }

    fun sinkMethod(): String {
        // ruleid: kotlin_constructor_sqli
        val query = sink(this.taintMethod())
        return query
    }
}

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

    return
}
