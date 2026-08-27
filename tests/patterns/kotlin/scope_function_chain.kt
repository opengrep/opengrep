fun chain(a: Thing) {
    a.let { x -> f(x) }
    // ERROR:
    a.let { x -> f(x) }.let { y -> g(y) }
}
