fun test_original_example() {
    val history = getHistory("name", "owner")
    listOf(history).flatMap { node ->
        val changes = node
        // ruleid: test-hof-taint
        sink(changes)
        listOf(changes)
    }
}
