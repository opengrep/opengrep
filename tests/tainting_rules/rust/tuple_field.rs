fn f() {
    let t = (source(), 0);
    // ruleid: taint
    sink(t.0);
    // ok: taint
    sink(t.1);
}
