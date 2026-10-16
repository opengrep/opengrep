fn guarded_arm_does_not_fall_through(k: i32) {
    let mut y = 0;
    match k {
        n if n > 0 => { y = taint_source(); }
        _ => {
            // ok: taint-switch-case-rust
            sink(y);
        }
    }
}

fn failed_guard_reaches_later_arm(k: i32) {
    let x = taint_source();
    match k {
        n if n > 0 => { a(n); }
        _ => {
            // ruleid: taint-switch-case-rust
            sink(x);
        }
    }
}

fn guard_reads_the_binding() {
    let k = taint_source();
    match k {
        n if check(n) => {
            // ruleid: taint-switch-case-rust
            sink(n);
        }
        _ => {}
    }
}
