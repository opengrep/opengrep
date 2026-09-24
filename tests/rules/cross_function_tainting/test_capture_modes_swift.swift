func captureList() {
    var x = 0
    // ok: test-capture-modes-swift
    let l = { [x] in sink(x) }
    x = source()
    l()
}

func implicit() {
    var x = 0
    // ruleid: test-capture-modes-swift
    let l = { sink(x) }
    x = source()
    l()
}

func initialised() {
    // ruleid: test-capture-modes-swift
    let l = { [y = source()] in sink(y) }
    l()
}
