<?php

// PHP 8.5 clone-with is the clone it stands for, so taint has to flow through
// it just as it does through an ordinary clone.

function test() {
    $a = source();
    //ruleid: test-clone-with
    sink(clone $a);

    $b = source();
    //ruleid: test-clone-with
    sink(clone($b, ["p" => 1]));

    $c = source();
    $d = clone($c, ["p" => 1]);
    //ruleid: test-clone-with
    sink($d);

    // tainted through the replacement properties rather than the object
    $e = source();
    //ruleid: test-clone-with
    sink(clone($safe, ["p" => $e]));

    $f = safe();
    //OK:
    sink(clone($f, ["p" => 1]));
}
