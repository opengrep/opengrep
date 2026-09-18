package sanitiser_active

fun sendClean(value: String) {
    val safe = sanitize(value)
    // ok: sanitiser-active-kotlin
    sink(safe)
}

fun sendDirty(value: String) {
    // ruleid: sanitiser-active-kotlin
    sink(value)
}
