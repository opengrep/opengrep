fn f(x: i32) {
    // ERROR:
    let a = move || use_it(x);
    let b = || use_it(x);
}
