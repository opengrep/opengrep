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
