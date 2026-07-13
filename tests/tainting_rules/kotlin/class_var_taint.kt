// The source pattern matches a `private val` populated from a method
// parameter; every such class member should become a taint source.
class Controller {

    private val a = mutableListOf<String>()
    private val b = mutableListOf<String>()
    private val c = mutableListOf<String>()

    fun addA(p: String): Boolean {
        a.add(p)
        return true
    }

    fun addB(p: String): Boolean {
        b.add(p)
        return true
    }

    fun useA(): String {
        // ruleid: taint
        sink(a)
        return ""
    }

    fun useB(): String {
        // ruleid: taint
        sink(b)
        return ""
    }

    fun useC(): String {
        // c is never populated from a parameter, so it is not a source
        // ok: taint
        sink(c)
        return ""
    }
}
