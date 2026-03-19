function outer() {
    var x = source();

    function inner() {
        // ruleid: test-nested-func-closure
        sink(x);
    }
}
