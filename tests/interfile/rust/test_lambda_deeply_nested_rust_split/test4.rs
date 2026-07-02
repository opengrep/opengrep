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
