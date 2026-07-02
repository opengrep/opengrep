package constructor_taint

fun sink(x: String) {
    println(x)
}

class Store(val data: String) {
    fun send() {
        // ruleid: test-constructor-taint
        sink(this.data)
    }
}
