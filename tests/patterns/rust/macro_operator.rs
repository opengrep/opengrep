fn f(a: i32, b: i32) {
    // MATCH:
    assert!(a == b);
    assert!(a != b);
    assert!(a < b);
    assert!(a + b);
}
