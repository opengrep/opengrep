<?php

// the '&' of a bitwise and, of a by-reference binding and of an intersection
// type are told apart, so a rule for one does not find the others

class K
{
    public static $s;

    public function m($b, $c)
    {
        //ERROR:
        $bitwise = $b & $c;

        //ERROR: the right operand need not be a variable
        $constant = $b & self::$s;

        // OK: a reference to a variable
        $ref = &$b;

        // OK: a reference to a static property
        $refStatic = &self::$s;

        return [$bitwise, $constant, $ref, $refStatic];
    }

    // OK: a by-reference parameter
    public function byRef(X &$p) {}

    // OK: an intersection type
    public function intersect(X&Y $q) {}

    // OK: a by-reference variadic
    public function variadic(X &...$r) {}
}

// OK: a reference-returning function
function &makeRef() { return $GLOBALS; }
