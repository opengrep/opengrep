package a

fun Payload.handle(x: String) {
    // ruleid: extension-function-on-receiver
    sink(x)
}
