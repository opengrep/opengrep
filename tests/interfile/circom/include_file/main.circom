pragma circom 2.0.0;

include "lib.circom";

function source() {
    return 1;
}

template Main() {
    signal output tainted;
    signal output clean;
    tainted <== relay(source());
    clean <== quiet(2);
}
