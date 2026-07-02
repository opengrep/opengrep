fun test_direct_call() {
    directCall({ x ->
        // ruleid: test-hof-taint
        sink(x)
    }, source())
}
