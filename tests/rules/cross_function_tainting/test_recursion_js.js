// Three-function cycle p -> q -> r -> p. Without an SCC fixed-point pass,
// any single visit order leaves at least one cycle member with an
// incomplete signature, and sink(p(0)) is missed.
function p(x) { return q(x); }
function q(x) { return r(source()); }
function r(x) {
    if (cond()) return p(x);
    return x;
}
function test_3cycle() {
    // ruleid: test-recursion-fixpoint
    sink(p(0));
}

// Self-recursion counter: source is in the BASE case (at n === 5), and
// the recursive case does NOT carry source through its arguments. Once
// guarded effects land in the engine, this test will require both
// guards AND the SCC fixed-point to fire correctly:
//   * Without guards, conservative analysis records ToReturn(source)
//     unconditionally, so sink(counter(0)) fires by over-approximation.
//   * With guards but a single pass, iter 0 stamps the base-case effect
//     with [guard n === 5]; at sink(counter(0)) the guard folds to
//     0 === 5 = false and the effect is dropped (false negative).
//   * With guards AND fixed-point iteration, each iteration substitutes
//     the previous iteration's sig through one more recursive frame,
//     accumulating guard variants n === 4, n === 3, ..., n === 0.
//     sink(counter(0)) eventually finds a guard that holds and fires.
// Today (no guards yet), this test passes by conservative
// over-approximation -- it serves as a regression check for the
// fixpoint engine's self-loop handling.
function counter(n) {
    if (n === 5) return source();
    return counter(n + 1);
}
function test_counter() {
    // ruleid: test-recursion-fixpoint
    sink(counter(0));
}
