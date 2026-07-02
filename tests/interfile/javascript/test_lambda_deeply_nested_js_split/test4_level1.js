function test4_level1(x) {
    let level2 = () => {
        let level3 = () => {
            // ruleid: test-lambda-deeply-nested-js
            sink(x);
        };
        level3();
    };
    level2();
}
