<?php

// PHP 8.5 attributes on constants, and PHP 8.0 attributes on class
// constants, must be attached to the constant definition in the generic AST

//MATCH:
#[Deprecated]
const OLD = 1;

//OK: no attribute
const FINE = 2;

//OK: different attribute
#[Other]
const OTHER = 3;

class K
{
    //MATCH:
    #[Deprecated]
    const OLD_K = 4;

    //OK: no attribute
    const FINE_K = 5;

    //OK: different attribute
    #[Other]
    const OTHER_K = 6;
}
