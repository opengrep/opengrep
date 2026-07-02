package constructor_taint

fun source(): String {
    return System.getenv("SECRET")
}

fun main() {
    val tainted = source()
    val s = Store(data = tainted)
    s.send()
}
