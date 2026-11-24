fn f() {
    let a = || {};
    // MATCH:
    let b = static || {};
}
