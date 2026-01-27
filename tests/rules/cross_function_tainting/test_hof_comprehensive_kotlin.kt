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

// ===== Top-level HOF Tests =====
// These test HOF callback detection at top level (outside any function)

// Top-level lambda callback
// ruleid: test-hof-taint
val toplevelSink = { x: String -> sink(x) }
val toplevelResult1 = toplevelSink(source())

// Top-level method HOF (forEach with named callback)
fun toplevelHandler(x: String) {
    // ruleid: test-hof-taint
    sink(x)
}

val toplevelItems = listOf(source())
val toplevelResult2 = toplevelItems.forEach(::toplevelHandler)

// Top-level user-defined HOF
val toplevelResult3 = directCall(::toplevelHandler, source())
