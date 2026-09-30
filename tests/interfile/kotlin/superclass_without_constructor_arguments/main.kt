package app

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

open class Base {
    open fun handle(x: String) {
        // ruleid: superclass-without-constructor-arguments
        sink(x)
    }
}

class D : Base {
    constructor(n: Int) : super()
}

fun run() {
    D(1).handle(source())
}
