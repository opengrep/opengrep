package app

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

open class Base {
    open fun handle(x: String) {
        // ok: superclass-without-constructor-arguments-control
        sink(x)
    }
}

class D : Base {
    constructor(n: Int) : super()

    override fun handle(x: String) {}
}

fun run() {
    D(1).handle(source())
}
