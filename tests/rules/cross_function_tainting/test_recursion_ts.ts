function p(x: any): any { return q(x); }
function q(x: any): any { return r(source()); }
function r(x: any): any {
    if (cond()) return p(x);
    return x;
}
function test_3cycle() {
    // ruleid: test-recursion-fixpoint
    sink(p(0));
}
