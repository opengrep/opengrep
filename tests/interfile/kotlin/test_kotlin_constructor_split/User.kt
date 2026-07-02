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
