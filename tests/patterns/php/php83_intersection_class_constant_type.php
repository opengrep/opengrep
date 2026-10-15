<?php

// a class constant type may be an intersection

class A
{
    //MATCH:
    const Countable&Stringable A = C;

    //MATCH:
    public const Countable&Stringable B = C, D = E;

    //OK: a union
    const Countable|Stringable F = C;

    //OK: one of the two only
    const Countable G = C;

    //OK: a DNF type
    const (Countable&Stringable)|null H = C;

    //OK: no type
    const I = C;
}
