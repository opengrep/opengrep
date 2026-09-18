<?php

// One branch leaves $x clean (Cell(Clean, Bot)), the other taints a field
// under it (Cell(None, Obj{f -> Tainted})). unify_cell used to union the
// xtaints (Clean U None = Clean) and unify the shapes (Bot U Obj = Obj)
// independently, yielding the invariant-breaking Cell(Clean, Obj ...) whose
// Clean short-circuits every offset lookup - a false negative on $x['f'].
function test_clean_join($cond) {
    $x = array();
    if ($cond) {
        $x = sanitize(input());
    } else {
        $x['f'] = source();
    }
    // ruleid: clean-join-taint
    sink($x['f']);
}

// Both branches clean: no finding.
function test_both_clean($cond) {
    $x = array();
    if ($cond) {
        $x = sanitize(input());
    } else {
        $x = sanitize(source());
    }
    // ok: clean-join-taint
    sink($x['f']);
}
