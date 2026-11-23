fn f() {
    let a = |x| x;
    // MATCH:
    let b = move |x| x;
}
