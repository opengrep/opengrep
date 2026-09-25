// The class header is the primary constructor: a construction passes its
// arguments to the header's parameters, and the init blocks and property
// initialisers run with them in source order.
class Foo(x: String) {
    init {
        // ruleid: kotlin_primary_constructor_init
        sink(x)
    }
}

class Bar(x: String) {
    val y = x
    init {
        // ruleid: kotlin_primary_constructor_init
        sink(y)
    }
}

class Baz(x: String) {
    init {
        // ok: kotlin_primary_constructor_init
        sink(x)
    }
}

fun direct() {
    Foo(source())
    Bar(source())
    Baz("constant")
}
