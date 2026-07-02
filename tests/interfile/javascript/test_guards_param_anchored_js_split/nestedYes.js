function nestedYes(y, x) {
    if (y.outer.inner) {
        // ruleid: test-guards-param-anchored-js
        sink(x);
    }
}
