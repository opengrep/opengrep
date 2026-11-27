pragma circom 2.0.0;

function sink(x) {
    return x;
}

function relay(x) {
    // ruleid: test-circom-include
    return sink(x);
}

function quiet(x) {
    // ok: test-circom-include
    return sink(x);
}
