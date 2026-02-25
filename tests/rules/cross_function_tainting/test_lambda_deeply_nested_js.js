// Test: Deeply nested lambdas (3 levels)
function test4() {
    let x = source();
    let level1 = () => {
        let level2 = () => {
            let level3 = () => {
                // ruleid: test-lambda-deeply-nested-js
                sink(x);
            };
            level3();
        };
        level2();
    };
    level1();
}

// Test: Deeply nested lambdas split across functions
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

function test4_caller() {
    let x = source();
    test4_level1(x);
}

function source() { return "tainted"; }
function sink(x) {}
