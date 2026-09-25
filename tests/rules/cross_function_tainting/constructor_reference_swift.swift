// The expression Foo.init refers to the initialiser of Foo, which a call
// through the function value runs.
class Foo {
    init(_ x: String) {
        // ruleid: constructor_reference_swift
        sink(x)
    }
}

class Bar {
    init(_ x: String) {
        // ok: constructor_reference_swift
        sink(x)
    }
}

func make(_ k: (String) -> Any) -> Any {
    return k(source())
}

func run() {
    make(Foo.init)
}
