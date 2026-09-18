<?php

// The body of a short lambda is an implicit return: taint entering the
// callback must flow back out through its result.
function test_identity_arrow() {
    $f = fn($x) => $x;
    $y = $f(source());
    // ruleid: arrow-fn-taint
    sink($y);
}

// An operator inside the arrow-fn body used to be lowered in void context,
// turning `===` into a bogus `obj.===(...)` pseudo method call whose
// ToSinkInCall effect could never resolve ("Could not find the shape of
// '$issues.==='"). Taint must still flow through array_filter, and the
// comparison operand must not become a spurious callee.
function test_array_filter_comparison() {
    $issues = source();
    $open = array_filter($issues, fn($issue) => $issue['status'] === 'open');
    foreach ($open as $i) {
        // ruleid: arrow-fn-taint
        sink($i);
    }
}

// A clean array filtered by a comparison stays clean.
function test_array_filter_clean() {
    $issues = array(1, 2, 3);
    $open = array_filter($issues, fn($issue) => $issue === 1);
    foreach ($open as $i) {
        // ok: arrow-fn-taint
        sink($i);
    }
}
