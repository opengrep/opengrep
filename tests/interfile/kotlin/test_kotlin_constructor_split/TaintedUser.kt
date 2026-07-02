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
