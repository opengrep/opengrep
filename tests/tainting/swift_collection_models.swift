// Test: Swift collection models for taint propagation

func testArrayAppend(tainted: String) {
    var arr = [String]()
    arr.append(tainted)
    // ruleid: swift-collection-taint
    sink(arr)
}

func testArrayPopLast(tainted: String) {
    var arr = [tainted]
    let value = arr.popLast()
    // ruleid: swift-collection-taint
    sink(value)
}

func testArrayFirst(tainted: String) {
    let arr = [tainted]
    let value = arr.first
    // ruleid: swift-collection-taint
    sink(value)
}

func testArrayRemoveFirst(tainted: String) {
    var arr = [tainted]
    let value = arr.removeFirst()
    // ruleid: swift-collection-taint
    sink(value)
}

func sink(_ data: Any?) {
    print(data as Any)
}
