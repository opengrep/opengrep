package app

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

interface I {
    fun handle(x: String)
}

class B : I {
    override fun handle(x: String) {
        // ruleid: delegation-forwards-control
        sink(x)
    }
}

class D(val b: B) : I {
    override fun handle(x: String) = b.handle(x)
}

fun run(d: D) {
    d.handle(source())
}
