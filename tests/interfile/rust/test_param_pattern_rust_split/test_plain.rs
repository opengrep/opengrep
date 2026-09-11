fn test_plain() {
    let cb = |v: String| {
        // ruleid: test-param-pattern-taint
        sink(v);
    };
    cb(source());
}
