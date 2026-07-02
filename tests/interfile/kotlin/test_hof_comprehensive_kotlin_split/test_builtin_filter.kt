fun test_builtin_filter() {
    val arr = listOf(source())
    arr.filter { x ->
        // ruleid: test-hof-taint
        sink(x)
        true
    }
}
