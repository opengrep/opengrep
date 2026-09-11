package a

class Payload {
    fun keep(x: String) {
        // ok: extension-function-on-receiver
        sink(x)
    }
}
