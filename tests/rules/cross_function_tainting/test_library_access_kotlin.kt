// Cross-function field-sensitive taint through Kotlin's
// kotlin.collections.MutableMap library-call recognisers. Maps flow
// across function boundaries as parameters / returns, and field-
// sensitivity must survive the call. Also exercises [getOrElse] —
// the trailing-lambda form — whose lambda return requires the
// intrafile analyser.

fun writeBody(m: MutableMap<String, String>, v: String) {
    m.put("body", v)
}

fun writeUser(m: MutableMap<String, String>, v: String) {
    m.put("user", v)
}

fun readBody(m: MutableMap<String, String>): String? {
    return m.get("body")
}

fun CrossPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    writeBody(m, source())
    // ruleid: test-library-access-taint
    sink(m.get("body"))
}

fun CrossNegSibling() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", "safe")
    writeUser(m, source())
    // ok: test-library-access-taint
    sink(m.get("body"))
}

fun CrossReadPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", source())
    // ruleid: test-library-access-taint
    sink(readBody(m))
}

fun GetOrElseCellTainted() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", source())
    // ruleid: test-library-access-taint
    sink(m.getOrElse("body") { "fallback" })
}

fun GetOrElseLambdaTainted() {
    val m: HashMap<String, String> = HashMap<String, String>()
    // ruleid: test-library-access-taint
    sink(m.getOrElse("body") { source() })
}

fun GetOrElseSiblingClean() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("user", source())
    // ok: test-library-access-taint
    sink(m.getOrElse("body") { "fallback" })
}
