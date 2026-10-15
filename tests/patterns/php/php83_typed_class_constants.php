<?php

// PHP 8.3: the type of a class constant reaches the generic AST

class A
{
    //MATCH:
    const int A = 1;

    //MATCH:
    public const int B = 2, C = 3;

    //OK: a different type
    const string D = 'd';

    //OK: no type
    const E = 4;
}
