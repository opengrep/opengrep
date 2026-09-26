package app

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

interface I {
    fun handle(x: String)
}

class B : I {
    override fun handle(x: String) {
        // ruleid: delegation-forwards
        sink(x)
    }
}

class D(b: B) : I by b

fun run(d: D) {
    d.handle(source())
}
