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
