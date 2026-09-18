package c

fun source(): String = "tainted"

fun use(s: a.Store) {
    s.run(source())
}
