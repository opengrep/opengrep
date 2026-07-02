package inheritance_basic

fun source(): String {
    return System.getenv("SECRET")
}

fun main() {
    val tainted = source()
    val d = Dog()
    d.process(tainted)
}
