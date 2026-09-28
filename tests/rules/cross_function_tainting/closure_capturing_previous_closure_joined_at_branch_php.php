<?php
function wrap($v, $f) {
    return function ($k) use ($v, $f) { return $k ? $f : $v; };
}

function joined($items, $c) {
    $f = wrap("safe", null);
    foreach ($items as $i) {
        $f = wrap("safe", $f);
    }
    if ($c) {
        $f = wrap("safe", wrap(source(), $f));
    }
    // ruleid: closure_capturing_previous_closure_joined_at_branch_php
    sink($f(1)(0));
    // ok: closure_capturing_previous_closure_joined_at_branch_php
    sink($f(0));
}

function straight($items) {
    $f = wrap("safe", null);
    foreach ($items as $i) {
        $f = wrap("safe", $f);
    }
    $f = wrap("safe", wrap(source(), $f));
    // ruleid: closure_capturing_previous_closure_joined_at_branch_php
    sink($f(1)(0));
    // ok: closure_capturing_previous_closure_joined_at_branch_php
    sink($f(0));
}
