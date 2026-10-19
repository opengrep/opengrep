fn f() {
    //ERROR: match
    let a = move |x| x + 1;
    // OK:
    let b = |x| x + 1;
}
