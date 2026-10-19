// A property holds the lambda assigned last: a method replaces the
// initialiser.
class C {
    var f = { x: String -> "safe" }
    fun setup() { f = { x: String -> x } }
    fun m() {
        // ruleid: kotlin_reassigned_field_lambda
        sink(f(source()))
    }
}
