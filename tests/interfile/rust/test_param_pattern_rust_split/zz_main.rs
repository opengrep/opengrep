// Rust closure with a tuple-destructuring parameter produces
// G.ParamPattern(PatTyped(PatTuple[a, _], _)). Taint from source() must
// route through the closure application and bind onto `a`.


// Baseline: plain single-param closure. Passes today.
