// The callable reference ::Foo refers to the constructor of Foo, which a
// call through the function value runs.
class Foo {
    constructor(x: String) {
        // ruleid: constructor_reference_kotlin
        sink(x)
    }
}

class Bar {
    constructor(x: String) {
        // ok: constructor_reference_kotlin
        sink(x)
    }
}

fun make(k: (String) -> Any): Any {
    return k(source())
}

fun run() {
    make(::Foo)
}
