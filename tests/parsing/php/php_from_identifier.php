<?php

// 'from' is only a keyword directly after 'yield'; everywhere else it is an
// ordinary identifier.

function from()
{
    return 1;
}

class C
{
    const from = 1;

    public static $from = 2;

    public function from()
    {
        return self::from;
    }
}

$a = from();
$b = (new C())->from();
$c = C::from();
$d = C::from;
$e = ["from" => from];

function g($gen)
{
    yield from $gen;
    yield from from();
    yield FROM [1, 2];

    yield
      from $gen;

    // an identifier that merely starts with 'from'
    yield fromage;
    $f = fromage;
}
