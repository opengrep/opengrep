// Kotlin: nested lambdas invoked via .invoke()
fun test_invoke() {
    val x = source()
    val outer: () -> Unit = {
        val inner: () -> Unit = {
            // ruleid: test-invoke-methods-kotlin
            sink(x)
        }
        inner.invoke()
    }
    outer.invoke()
}

// Negative: no taint
fun test_no_taint() {
    val x = "clean"
    val outer: () -> Unit = {
        val inner: () -> Unit = {
            // ok: test-invoke-methods-kotlin
            sink(x)
        }
        inner.invoke()
    }
    outer.invoke()
}

fun source(): String = "tainted"
fun sink(x: String) {}
