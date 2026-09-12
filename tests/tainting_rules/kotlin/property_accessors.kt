class Accessors {
    // ruleid: taint
    val inlineGetter: String get() = sink(source())

    val expressionGetter: String
        // ruleid: taint
        get() = sink(source())

    val blockGetter: String
        get() {
            val value = source()
            // ruleid: taint
            return sink(value)
        }

    var expressionSetter: String = ""
        // ruleid: taint
        set(value) = consume(sink(source()))

    var blockSetter: String = ""
        set(value) {
            val tainted = source()
            // ruleid: taint
            field = sink(tainted)
        }

    var fromParameter: String = ""
        set(value: String) {
            val copy = value
            // ruleid: taint
            field = sink(copy)
        }

    val safeGetter: String
        get() {
            // ok: taint
            return sink("safe")
        }

    var safeSetter: String = ""
        set(value) {
            // ok: taint
            field = sink("safe")
        }
}

// ruleid: taint
val topLevel: String get() = sink(source())
