class R(val path: String, val other: String)

fun optionalValue() {
    val r: R? = R(source(), "x")
    // ruleid: not_null_assertion_kotlin
    sink(r!!.path)
    // ok: not_null_assertion_kotlin
    sink(r!!.other)
}
