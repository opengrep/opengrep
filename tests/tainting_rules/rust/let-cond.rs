fn f(some_x: Option<String>) {
    // ok
    let z = sink(x) 
    if let Some(x) = some_x {
        // ruleid: taint
        sink(x)
    } else {
        // ok
        sink(x)
        // should not hit since x is not defined here!
    }
}

fn g(some_x: Option<String>, some_y: Option<String>) {
    // ok
    let z = sink(x)
    if let (Some(x), Some(y)) = (some_x, some_y) {
        // ruleid: taint
        let _ = sink(x);
        // ruleid: taint
        sink(y)
    } else {
        // ok
        sink(x)
    }
}
