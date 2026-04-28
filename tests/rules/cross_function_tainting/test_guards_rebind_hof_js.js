// HOF guard rebinding across a JavaScript call chain. Inner's
// [ToSinkInCall] (the callback invocation) is guarded by a branch
// condition on one of its parameters. Outer forwards both the callback
// and the guard-relevant parameter to inner. At top-level
// instantiation, the guard should drop the effect when the
// top-level-caller argument fails the condition.


function source() {
    return "taint";
}

function sink(_x) {
}


// ---------- No finding: top-level opts.flag is false ----------

function doSinkNo(v) {
    sink(v);
}

function innerHofNo(a, cb, b, opts, c, x) {              // cb=1, opts=3, x=5
    if (opts.flag) {
        cb(x);
    }
}

function outerHofNo(d, e, myCb, f, myOpts, g, myX) {     // myCb=2, myOpts=4, myX=6
    innerHofNo("a", myCb, "b", myOpts, "c", myX);
}

function callHofNo() {
    outerHofNo("d", "e", doSinkNo, "f", {flag: false}, "g", source());
}


// ---------- Finding expected: top-level opts.flag is true ----------

function doSinkYes(v) {
    // ruleid: test-guards-rebind-hof-js
    sink(v);
}

function innerHofYes(a, cb, b, opts, c, x) {
    if (opts.flag) {
        cb(x);
    }
}

function outerHofYes(d, e, myCb, f, myOpts, g, myX) {
    innerHofYes("a", myCb, "b", myOpts, "c", myX);
}

function callHofYes() {
    outerHofYes("d", "e", doSinkYes, "f", {flag: true}, "g", source());
}
