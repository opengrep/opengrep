fn break_leaves_the_loop(ks: Vec<i32>) {
    let mut y = 0;
    for k in ks {
        match k {
            1 => { y = taint_source(); break; }
            _ => {}
        }
        y = 0;
    }
    // ruleid: taint-switch-case-rust
    sink(y);
}

fn break_skips_the_rest_of_the_loop(ks: Vec<i32>) {
    for k in ks {
        let mut y = 0;
        match k {
            1 => { y = taint_source(); break; }
            _ => {}
        }
        // ok: taint-switch-case-rust
        sink(y);
    }
}
