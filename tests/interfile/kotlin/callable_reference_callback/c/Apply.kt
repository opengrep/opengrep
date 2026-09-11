package c

fun directCall(callback: (String) -> Unit, value: String) {
    callback(value)
}
