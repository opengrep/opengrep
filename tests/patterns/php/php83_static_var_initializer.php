<?php

// PHP 8.3: a static variable can be initialized with any expression

function f($i, $throw) {
    //MATCH:
    static $a = bar();

    //MATCH:
    static $b = 1, $c = bar($i);

    //MATCH:
    static $d = bar($i <= 10 ? f($i + 1) : "Done $i");

    //OK: a different call
    static $e = baz();

    //OK: a constant expression
    static $g = 1 + 2;

    //OK: no initializer
    static $h;

    //OK: a variable
    static $j = $i;

    //OK: a ternary with a throw
    static $k = $throw ? (throw new Exception('no')) : 42;

    //OK: a dynamic class name
    static $l = new (FOO);

    //OK: a closure
    static $m = function () { return bar(); };
}
