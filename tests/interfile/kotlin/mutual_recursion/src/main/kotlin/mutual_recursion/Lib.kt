package mutual_recursion

fun cond(): Boolean {
    return false
}

fun p(x: Any): Any {
    return q(x)
}

fun q(x: Any): Any {
    return r(source())
}

fun r(x: Any): Any {
    if (cond()) {
        return p(x)
    }
    return x
}
