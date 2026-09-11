package a

class Other {
    fun handle(x: String) {
        // ok: extension-function-on-receiver
        sink(x)
    }
}
