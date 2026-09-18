<?php

function build($x, $n) {
    $node = new stdClass();
    if ($n > 0) {
        $node->a = build($x, $n - 1);
        $node->b = build($x, $n - 1);
        $node->c = build($x, $n - 1);
        $node->d = build($x, $n - 1);
    }
    $node->v = $x;
    return $node;
}

function tainted() {
    $t = build(source(), 3);
    // ruleid: recursive-builder-offsets
    sink($t->a->b->v);
}

function clean() {
    $t = build(1, 3);
    // ok: recursive-builder-offsets
    sink($t->a->b->v);
}
