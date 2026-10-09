<?php

// PHP 8.5: a copy made by clone, in any of its forms, carries the taint of
// the object and of the replaced properties

function test() {
    $a = clone(object: source());
    //ruleid: test-clone-function
    sink($a);

    $b = clone(object: $x, withProperties: ['p' => source()]);
    //ruleid: test-clone-function
    sink($b);

    $c = \clone(source());
    //ruleid: test-clone-function
    sink($c);

    $d = clone(...[source()]);
    //ruleid: test-clone-function
    sink($d);

    $e = clone(object: safe(), withProperties: ['p' => 1]);
    //OK: test-clone-function
    sink($e);
}
