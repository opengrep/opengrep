<?php

// PHP 8.3: a static variable can be initialized with any expression, so taint
// flows from its initializer

function test($cond) {
    static $a = source();
    //ruleid: test-static-var-initializer
    sink($a);

    static $b = 1, $c = source();
    //ruleid: test-static-var-initializer
    sink($c);

    static $d = $cond ? source() : 1;
    //ruleid: test-static-var-initializer
    sink($d);

    static $e = escape(source());
    //ruleid: test-static-var-initializer
    sink($e);

    static $f = safe();
    //OK: test-static-var-initializer
    sink($f);

    //OK: test-static-var-initializer
    sink($b);
}
