<?php

// PHP 8.5: a constant expression may contain a cast

//MATCH:
const A = (int) 0.3;

//MATCH: a cast binds tighter than '+'
const B = (int) "3" + 1;

class K
{
    //MATCH:
    const C = (int) self::X;

    //MATCH:
    public $p = (int) "3";

    public function f(
        //MATCH:
        $x = (int) 1.5,
    ) {}

    //OK: a different cast
    const D = (string) 1;

    //OK: an object cast
    public $o = (object) [];
}

enum E: int
{
    //MATCH:
    case A = (int) 1.0;
}

//OK: no cast
const F = 1;
