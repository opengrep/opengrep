const CMD: i32 = 7;
const FOO: i32 = 1;
struct Unit;
enum E { A, B(i32) }
use E::A;

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

fn constant_binding(mode: i32) {
    // ruleid: switch-case-constant-propagation-rust
    exec(CMD);
    match mode {
        CMD => {
            // ruleid: switch-case-constant-propagation-rust
            exec(CMD);
        }
        _ => {}
    }
    // ruleid: switch-case-constant-propagation-rust
    exec(CMD);
}

fn later_arm_reachable(k: i32) {
    let x = taint_source();
    match k {
        FOO => {}
        _ => {
            // ruleid: taint-switch-case-rust
            sink(x);
        }
    }
}

fn label_is_not_bound() {
    let k = taint_source();
    match k {
        FOO => {
            // ok: taint-switch-case-rust
            sink(FOO);
        }
        _ => {}
    }
}

fn binder_still_binds() {
    let k = taint_source();
    match k {
        FOO => {}
        n => {
            // ruleid: taint-switch-case-rust
            sink(n);
        }
    }
}

fn local_variable_does_not_make_a_value() {
    let k = taint_source();
    let shadow = 1;
    match k {
        shadow => {
            // ruleid: taint-switch-case-rust
            sink(shadow);
        }
    }
}

fn local_const(k: i32) {
    let x = taint_source();
    const LOCAL: i32 = 5;
    match k {
        LOCAL => {}
        _ => {
            // ruleid: taint-switch-case-rust
            sink(x);
        }
    }
}

fn unit_struct_is_not_bound() {
    let u = taint_source();
    match u {
        Unit => {
            // ok: taint-switch-case-rust
            sink(Unit);
        }
    }
}

fn imported_variant_is_not_bound() {
    let e = taint_source();
    match e {
        A => {
            // todook: taint-switch-case-rust
            sink(A);
        }
        _ => {}
    }
}
