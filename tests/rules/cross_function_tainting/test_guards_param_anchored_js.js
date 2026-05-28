// Parameter-anchored branch guards via aliased object literal at the
// call site. The caller binds a literal [{...}] to a local, and the
// callee branches on a field of the parameter. At instantiation, the
// svalue walker must step through the local's [id_svalue] (a [G.Sym]
// of a [G.Container Dict]) to recover the literal at the branch path.


function source() {
    return "taint";
}

function sink(_x) {
}


// ---------- Direct field truthiness (Bool literal at call site) ----------

function fieldNo(opts, x) {
    if (opts.flag) {
        // ok: test-guards-param-anchored-js
        sink(x);
    }
}

function callFieldNo() {
    const opts = {flag: false};
    fieldNo(opts, source());
}


function fieldYes(opts, x) {
    if (opts.flag) {
        // ruleid: test-guards-param-anchored-js
        sink(x);
    }
}

function callFieldYes() {
    const opts = {flag: true};
    fieldYes(opts, source());
}


// ---------- Field equality cond (Operator(Eq, [Fetch opts.code; Lit N])) ----------

function eqNo(opts, x) {
    if (opts.code === 0) {
        // ok: test-guards-param-anchored-js
        sink(x);
    }
}

function callEqNo() {
    const opts = {code: 1};
    eqNo(opts, source());
}


function eqYes(opts, x) {
    if (opts.code === 0) {
        // ruleid: test-guards-param-anchored-js
        sink(x);
    }
}

function callEqYes() {
    const opts = {code: 0};
    eqYes(opts, source());
}


// ---------- Nested field, two levels ----------

function nestedNo(y, x) {
    if (y.outer.inner) {
        // ok: test-guards-param-anchored-js
        sink(x);
    }
}

function callNestedNo() {
    const y = {outer: {inner: false}};
    nestedNo(y, source());
}


function nestedYes(y, x) {
    if (y.outer.inner) {
        // ruleid: test-guards-param-anchored-js
        sink(x);
    }
}

function callNestedYes() {
    const y = {outer: {inner: true}};
    nestedYes(y, source());
}
