fun apply2(cb1: (String) -> Unit, cb2: (String) -> Unit, data: String) {
    cb2(data)
}

fun keywordsReordered() {
    // ruleid: kotlin_named_callback_argument
    apply2(data = source(), cb2 = { x -> sink(x) }, cb1 = { y -> })
}

fun uncalledCallbackSinks() {
    // ok: kotlin_named_callback_argument
    apply2(cb2 = { y -> }, cb1 = { x -> sink(x) }, data = source())
}
