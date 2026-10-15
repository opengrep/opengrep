<?php

function readonly($readonly, $current = true, $display = true)
{
    return $readonly === $current ? ' readonly' : '';
}

//MATCH:
readonly($disabled);

//MATCH:
echo readonly($a, true, false);

//MATCH: any case
echo READONLY($a);

//OK: a method of that name
$form->readonly($disabled);

//OK: a first-class callable, which does not call it
$f = readonly(...);

class A
{
    //OK: the modifier
    public readonly int $p;

    public function __construct(
        //OK: the modifier on a promoted parameter
        public readonly int $q,
    ) {}
}
