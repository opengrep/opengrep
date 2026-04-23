// Rust closure with a tuple-destructuring parameter produces
// G.ParamPattern(PatTyped(PatTuple[a, _], _)). Taint from source() must
// route through the closure application and bind onto `a`.

fn test_destructure() {
    // Sink on the SECOND tuple component: the enumeration fallback in
    // Fold_IL_params gives each destructured id its own Arg index, so `b`
    // maps to Arg 1 and is not bound by the single call argument. Taint
    // from source() needs proper destructure routing to reach `b`.
    let cb = |(_a, b): (String, String)| {
        // ruleid: test-param-pattern-taint
        sink(b);
    };
    cb(source());
}

// Baseline: plain single-param closure. Passes today.
fn test_plain() {
    let cb = |v: String| {
        // ruleid: test-param-pattern-taint
        sink(v);
    };
    cb(source());
}
