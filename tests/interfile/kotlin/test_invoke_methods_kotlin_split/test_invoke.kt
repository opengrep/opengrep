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
