function nestedNo(y, x) {
    if (y.outer.inner) {
        // ok: test-guards-param-anchored-js
        sink(x);
    }
}
