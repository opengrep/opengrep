<?php

function test() {
    $a = source();
    //ruleid: test-array-append
    sink($a);

    $b[] = source();
    //ruleid: test-array-append
    sink($b);

    $c[0] = source();
    //ruleid: test-array-append
    sink($c);

    $d[] = safe();
    //OK:
    sink($d);
}
