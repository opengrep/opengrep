fun constantPairs(p: String) {
    // ok: destructured-lambda-parameter
    listOf(Pair("a", "b")).forEach { (a, b) -> sink(a) }
}

fun taintedPairs() {
    // ruleid: destructured-lambda-parameter
    listOf(Pair(source(), "b")).forEach { (a, b) -> sink(a) }
}

fun main() {
    constantPairs(source())
}
