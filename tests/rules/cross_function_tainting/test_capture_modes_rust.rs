fn by_move() {
    let mut x = 0;
    // ok: test-capture-modes-rust
    let l = move || { sink(x); };
    x = source();
    l();
}

fn borrowed() {
    let mut x = 0;
    // ruleid: test-capture-modes-rust
    let l = || { sink(x); };
    x = source();
    l();
}
