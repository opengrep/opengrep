// Test: Rust variable shadowing taint propagation
// Verifies that taint correctly propagates through `let x = x;` patterns

fn test_no_shadowing(tainted: String) {
    let url = tainted;
    // ruleid: taint
    sink(url);
}

fn test_single_shadowing(tainted: String) {
    let url = tainted;
    let url = url;  // Variable shadowing
    // ruleid: taint
    sink(url);
}

fn test_multiple_shadowing(tainted: String) {
    let url = tainted;
    let url = url;
    let url = url;
    let url = url;
    // ruleid: taint
    sink(url);
}

fn test_shadowing_with_rename(tainted: String) {
    let url = tainted;
    let url2 = url;  // Different variable name (not shadowing)
    // ruleid: taint
    sink(url2);
}

fn test_shadowing_in_nested_scope(tainted: String) {
    let url = tainted;
    {
        let url = url;  // Shadowing in inner scope
        // ruleid: taint
        sink(url);
    }
}

fn test_clean_not_tainted(clean: String) {
    let url = clean;
    let url = url;
    // ok: taint
    sink(url);
}

fn sink(data: String) {
    println!("{}", data);
}
