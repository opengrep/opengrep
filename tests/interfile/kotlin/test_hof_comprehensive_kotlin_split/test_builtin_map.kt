fun test_builtin_map() {
    val arr = listOf(source())
    arr.map { x ->
        // ruleid: test-hof-taint
        sink(x)
        x
    }
}
