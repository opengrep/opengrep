pragma circom 2.0.0;

function sink(x) {
    return x;
}

function _relay(x) {
    // ruleid: test-circom-include-underscore
    return sink(x);
}

function _quiet(x) {
    // ok: test-circom-include-underscore
    return sink(x);
}
