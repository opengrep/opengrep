<?php

// an abstract method has no body, so the pattern ends at the semicolon

abstract class C {
    //ERROR:
    abstract protected function e($x);

    //ERROR: no visibility at all
    abstract function f($x);

    // OK: not abstract
    public function g($x) { echo $x; }

    // OK: final, not abstract
    final public function h($x) { echo $x; }
}
