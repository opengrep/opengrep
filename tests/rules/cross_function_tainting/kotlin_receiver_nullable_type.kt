class Impl {
    fun produce(): String {
        return source()
    }
}

class Other {
    fun produce(): String {
        return "constant"
    }
}

fun viaNullable(other: Impl?) {
    if (other != null) {
        // ruleid: kotlin_receiver_nullable_type
        sink(other.produce())
    }
}

fun viaNullableOther(other: Other?) {
    if (other != null) {
        // ok: kotlin_receiver_nullable_type
        sink(other.produce())
    }
}
