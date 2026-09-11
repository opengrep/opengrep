package inheritance_basic

fun sink(x: String) {
    println(x)
}

open class Animal {
    fun process(data: String) {
        // ruleid: test-inheritance-basic
        sink(data)
    }
}
