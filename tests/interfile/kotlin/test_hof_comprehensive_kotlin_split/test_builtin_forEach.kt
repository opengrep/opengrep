fun test_builtin_forEach() {
    val arr = listOf(source())
    arr.forEach { x ->
        // ruleid: test-hof-taint
        sink(x)
    }
}
