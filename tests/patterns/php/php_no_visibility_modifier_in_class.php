<?php

// a pattern that names no visibility matches a method of any visibility, so
// the implicit public of a modifier-less method must not be added to patterns

//ERROR:
class WithPrivate {
    private function a($x) { echo $x; }
}

//ERROR:
class WithPublic {
    public function b($x) { echo $x; }
}

//ERROR:
class WithPlain {
    function c($x) { echo $x; }
}

// OK: no method at all
class NoMethods {
    public $p = 1;
}
