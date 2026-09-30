package app

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

interface I {
    fun handle(x: String)
}

open class Base {
    open fun handle(x: String) {
        // ruleid: superclass-without-constructor-arguments-after-interface
        sink(x)
    }
}

class D : I, Base {
    constructor(n: Int) : super()
}

fun run() {
    D(1).handle(source())
}
