// Comprehensive HOF test for Kotlin: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====

fun <T> directCall(callback: (T) -> Unit, value: T) {
    callback(value)
}

// ===== Test Cases =====

fun test_direct_call() {
    directCall({ x ->
        // ruleid: test-hof-taint
        sink(x)
    }, source())
}

// ===== Built-in collection functions =====

fun test_builtin_map() {
    val arr = listOf(source())
    arr.map { x ->
        // ruleid: test-hof-taint
        sink(x)
        x
    }
}

fun test_builtin_forEach() {
    val arr = listOf(source())
    arr.forEach { x ->
        // ruleid: test-hof-taint
        sink(x)
    }
}

fun test_builtin_filter() {
    val arr = listOf(source())
    arr.filter { x ->
        // ruleid: test-hof-taint
        sink(x)
        true
    }
}

// ===== Complex Example =====

fun getHistory(name: String, owner: String): String {
    val result = source()
    return result
}

fun test_original_example() {
    val history = getHistory("name", "owner")
    listOf(history).flatMap { node ->
        val changes = node
        // ruleid: test-hof-taint
        sink(changes)
        listOf(changes)
    }
}

// Stub functions
fun source(): String = "tainted"
fun sink(s: String) {}
