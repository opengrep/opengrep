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
