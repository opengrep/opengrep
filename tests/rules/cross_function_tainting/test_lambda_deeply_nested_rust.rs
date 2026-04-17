// Test: Deeply nested lambdas (3 levels)
fn test4() {
    let x = source();
    let level1 = || {
        let level2 = || {
            let level3 = || {
                // ruleid: test-lambda-deeply-nested-rust
                sink(&x);
            };
            level3();
        };
        level2();
    };
    level1();
}

// Test: Deeply nested lambdas split across functions
fn test4_level1(x: String) {
    let level2 = || {
        let level3 = || {
            // ruleid: test-lambda-deeply-nested-rust
            sink(&x);
        };
        level3();
    };
    level2();
}

fn test4_caller() {
    let x = source();
    test4_level1(x);
}

fn source() -> String { String::from("tainted") }
fn sink(_s: &String) {}
