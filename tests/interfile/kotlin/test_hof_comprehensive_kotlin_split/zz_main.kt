// Comprehensive HOF test for Kotlin: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====

fun <T> directCall(callback: (T) -> Unit, value: T) {
    callback(value)
}

// ===== Test Cases =====


// ===== Built-in collection functions =====




// ===== Complex Example =====



// Stub functions
fun source(): String = "tainted"

// ===== Top-level HOF Tests =====
// These test HOF callback detection at top level (outside any function)

// Top-level lambda callback
// ruleid: test-hof-taint
val toplevelSink = { x: String -> sink(x) }
val toplevelResult1 = toplevelSink(source())

// Top-level method HOF (forEach with named callback)

val toplevelItems = listOf(source())
val toplevelResult2 = toplevelItems.forEach(::toplevelHandler)

// Top-level user-defined HOF
val toplevelResult3 = directCall(::toplevelHandler, source())
