package app

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

interface I {
    fun handle(x: String)
}

open class Base {
    open fun handle(x: String) {
        // ruleid: superclass-anywhere-in-list
        sink(x)
    }
}

class C : I, Base()

fun run() {
    C().handle(source())
}
