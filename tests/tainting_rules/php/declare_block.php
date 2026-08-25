<?php

// A declare block runs its body unconditionally and introduces no scope of its
// own, so taint has to cross its braces in both directions.
// coupling: ast_php_build.ml, the Declare case

function test() {
    declare(ticks=1) {
        $a = source();
    }
    //ruleid: test-declare-block
    sink($a);

    $b = source();
    declare(ticks=1) {
        //ruleid: test-declare-block
        sink($b);
    }

    declare(ticks=1) {
        $c = source();
        //ruleid: test-declare-block
        sink($c);
    }

    declare(strict_types=1) {
        $d = safe();
    }
    //OK:
    sink($d);
}
