<?php

// '>>' is a shift wherever a type cannot be: in a constant or default value,
// in an attribute argument, and after a '::class'

$c = Foo::class;

//MATCH:
$a = 4 >> 1;

class A
{
    //MATCH:
    const X = 4 >> 1;

    //MATCH:
    public $p = 8 >> 2;

    //MATCH:
    #[Attr(16 >> 3)]
    public function f(
        //MATCH:
        $x = 4 >> 1,
    ) {
        //MATCH:
        return $x >> 1;
    }

    //OK: a left shift
    const Y = 4 << 1;

    //OK: a comparison
    const Z = 4 > 1;
}

//MATCH:
function g($x = A::class, $y = 2 >> 1) {}
