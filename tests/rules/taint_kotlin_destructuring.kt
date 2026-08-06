// Destructuring declaration. The entity name is an EPattern holding a
// PatTuple; before it was lowered through pattern_assign_statements,
// `a` and `b` were never assigned in the IL.
fun declForm() {
    val (a, b) = taint_source()

    // ruleid: taint-kotlin-destructuring
    sink(a)

    // ruleid: taint-kotlin-destructuring
    sink(b)
}

// A single (non-destructuring) declaration keeps its existing lowering.
fun singleBinding() {
    val c = taint_source()

    // ruleid: taint-kotlin-destructuring
    sink(c)
}

fun clean() {
    val d = 1

    // ok: taint-kotlin-destructuring
    sink(d)
}
