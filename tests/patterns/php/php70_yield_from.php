<?php

function g($gen)
{
    //ERROR:
    yield from $gen;

    //ERROR:
    yield
      from $gen;

    //ERROR: as in PHP, 'from' is the keyword whatever follows it
    yield from($gen);

    // OK: delegating to a generator is not the same as yielding it
    yield $gen;

    // OK: 'from' outside a yield is an ordinary function
    $x = from($gen);
}
