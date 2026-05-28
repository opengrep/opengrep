// The `mut` keyword on a simple typed parameter rides on
// `parameter_classic.pattrs`; before lowering simple `Pat (PatId _)`
// to `G.Param`, the attribute was silently dropped by the `Pat`
// branch and the pattern below would not have matched.

// ERROR:
fn with_mut(mut x: String) {
    let _ = x;
}

fn without_mut(x: String) {
    let _ = x;
}
