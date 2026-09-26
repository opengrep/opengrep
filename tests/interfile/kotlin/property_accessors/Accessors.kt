package accessors

class Receiver

val Receiver.extensionValue: String
    get() {
        // ruleid: kotlin-property-accessor-interfile
        return sink(source())
    }

var Receiver.extensionPair: String
    get() = "safe"
    set(value) {
        // ruleid: kotlin-property-accessor-interfile
        sink(source())
    }

// ruleid: kotlin-property-accessor-interfile
val Receiver.semicolonValue: String; get() = sink(source())

class Payload {
    fun tainted(): String { return source() }
}

var stored: Payload = Payload()
    set(value) {
        // ruleid: kotlin-property-accessor-interfile
        sink(value.tainted())
    }

val secret: String
    get() { return source() }

fun get_secret(): String = "safe"

fun syntheticNameDoesNotBind() {
    // ok: kotlin-property-accessor-interfile
    sink(get_secret())
}

val withLocalHelper: String
    get() {
        fun accessorHelper(): String { return source() }
        // ruleid: kotlin-property-accessor-interfile
        return sink(accessorHelper())
    }

fun outsideAccessor() {
    // ok: kotlin-property-accessor-interfile
    sink(accessorHelper())
}

class MemberAccessors {
    val memberSecret: String
        get() { return source() }

    val checked: String
        get() {
            // ruleid: kotlin-property-accessor-interfile
            return sink(source())
        }

    fun get_memberSecret(): String = "safe"

    fun syntheticMemberNameDoesNotBind() {
        // ok: kotlin-property-accessor-interfile
        sink(get_memberSecret())
    }
}
