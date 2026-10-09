class Accessors {
    // ERROR:
    val inlineGetter: String get() = danger("inline getter")

    val expressionGetter: String
        // ERROR:
        get() = danger("expression getter")

    val blockGetter: String
        get(): String {
            // ERROR:
            return danger("block getter")
        }

    var expressionSetter: String = ""
        // ERROR:
        set(value) = consume(danger(value))

    // ERROR:
    var inlineSetter: String = ""; set(value) { field = danger(value) }

    var blockSetter: String = ""
        set(value: String) {
            // ERROR:
            field = danger(value)
        }

    var both: String
        // ERROR:
        get() = danger("getter with setter")
        set(value) {
            // ERROR:
            consume(danger(value))
        }

    var privateSetter: String = ""
        private set

    // ERROR:
    fun control() = danger("ordinary method")
}

// ERROR:
val topLevel: String get() = danger("top-level getter")

var topLevelSetter: String = ""
    set(value) {
        // ERROR:
        field = danger(value)
    }
