// Comprehensive HOF test for Swift: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====

func customMap<T>(_ arr: [T], _ callback: (T) -> T) -> [T] {
    var result: [T] = []
    for item in arr {
        result.append(callback(item))
    }
    return result
}

func customForEach<T>(_ arr: [T], _ callback: (T) -> Void) {
    for item in arr {
        callback(item)
    }
}

func directCall<T>(_ callback: (T) -> Void, _ value: T) {
    callback(value)
}

// ===== Test Cases =====

func test_custom_map() {
    let arr = [source()]
    customMap(arr) { x in
        // ruleid: test-hof-taint
        sink(x)
        return x
    }
}

func test_custom_foreach() {
    let arr = [source()]
    customForEach(arr) { x in
        // ruleid: test-hof-taint
        sink(x)
    }
}

func test_direct_call() {
    directCall({ x in
        // ruleid: test-hof-taint
        sink(x)
    }, source())
}

// ===== Built-in array methods =====

func test_builtin_map() {
    let arr = [source()]
    arr.map { x in
        // ruleid: test-hof-taint
        sink(x)
        return x
    }
}

func test_builtin_forEach() {
    let arr = [source()]
    arr.forEach { x in
        // ruleid: test-hof-taint
        sink(x)
    }
}

func test_builtin_filter() {
    let arr = [source()]
    arr.filter { x in
        // ruleid: test-hof-taint
        sink(x)
        return true
    }
}

// ===== Complex Example =====

func getHistory(_ name: String, _ owner: String) -> String {
    let result = source()
    return result
}

func test_original_example() {
    let history = getHistory("name", "owner")
    [history].flatMap { node in
        let changes = node
        // ruleid: test-hof-taint
        sink(changes)
        return [changes]
    }
}

// Stub functions
func source() -> String {
    return "tainted"
}

func sink(_ s: String) {
}

// ===== Top-level HOF Tests =====
// These test HOF callback detection at top level (outside any function)

// Top-level lambda callback
// ruleid: test-hof-taint
let toplevelSink: (String) -> Void = { x in sink(x) }
toplevelSink(source())

// Top-level method HOF (forEach with named callback)
func toplevelHandler(_ x: String) {
    // ruleid: test-hof-taint
    sink(x)
}

let toplevelItems = [source()]
toplevelItems.forEach(toplevelHandler)

// Top-level user-defined HOF
customForEach(toplevelItems, toplevelHandler)
