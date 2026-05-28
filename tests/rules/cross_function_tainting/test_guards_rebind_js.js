// Guard rebinding across a JavaScript call chain. At each intermediate
// frame the forwarded parameter sits at a different index so any
// indexing bug in the rebinding would surface.


function source() {
    return "taint";
}

function sink(_x) {
}


// ---------- No finding: top-level opts.flag is false ----------

function innerNo(a, opts, b, x) {              // opts=1, x=3
    if (opts.flag) {
        sink(x);
    }
}

function outerNo(c, d, p, e, x) {               // p=2, x=4
    innerNo("dummy_a", p, "dummy_b", x);
}

function callChainNo() {
    outerNo("c", "d", {flag: false}, "e", source());
}


// ---------- Finding expected: top-level opts.flag is true ----------

function innerYes(a, opts, b, x) {
    if (opts.flag) {
        // ruleid: test-guards-rebind-js
        sink(x);
    }
}

function outerYes(c, d, p, e, x) {
    innerYes("dummy_a", p, "dummy_b", x);
}

function callChainYes() {
    outerYes("c", "d", {flag: true}, "e", source());
}
