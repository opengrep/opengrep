// A call on an instance reaches an extension function declared for its
// class; a member function of the same name wins over the extension.
class A

class B {
    fun ext(x: String) {
        println(x)
    }
}

fun A.ext(x: String) {
    // ruleid: extension_function_kotlin
    sink(x)
}

fun B.ext(x: String) {
    // ok: extension_function_kotlin
    sink(x)
}

fun main() {
    A().ext(source())
    B().ext(source())
}
