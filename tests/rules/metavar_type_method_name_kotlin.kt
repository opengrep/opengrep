class PropertyFirst {
    val count: Int = 0

    fun count(): String {
        return ""
    }

    fun use() {
        // ruleid: call-is-the-method
        count()
        // ruleid: read-is-the-property
        sink(count)
    }
}

class MethodFirst {
    fun tally(): String {
        return ""
    }

    val tally: Int = 0

    fun use() {
        // ruleid: call-is-the-method
        tally()
        // ruleid: read-is-the-property
        sink(tally)
    }
}

class CallBeforeBoth {
    fun use() {
        // ruleid: call-is-the-method
        total()
        // ruleid: read-is-the-property
        sink(total)
    }

    val total: Int = 0

    fun total(): String {
        return ""
    }
}
