class Foo

fun f(x: Int): Int = x

fun make() {
    val g = ::f
    val c = ::Foo
}
