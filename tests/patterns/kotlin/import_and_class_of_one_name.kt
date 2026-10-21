import a.b.foo

class foo

fun g() {
    //ERROR: match
    foo()
}

class Outer {
    class foo

    fun m() {
        foo()
    }
}
