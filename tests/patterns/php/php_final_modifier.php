<?php

// 'final' and 'abstract' may lead a method pattern, in either order with a
// visibility modifier

abstract class C {
    //ERROR:
    final public function a($x) { echo $x; }

    //ERROR: the other order
    public final function b($x) { echo $x; }

    //ERROR: no visibility at all
    final function c($x) { echo $x; }

    // OK: not final
    public function d($x) { echo $x; }

    // OK: abstract, not final
    abstract protected function e($x);
}
