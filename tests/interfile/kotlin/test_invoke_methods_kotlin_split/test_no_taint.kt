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
