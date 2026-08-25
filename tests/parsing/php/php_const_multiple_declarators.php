<?php

// One 'const' statement may declare several constants, as class constants
// already could.

const A = 1, B = 2;
const C = 1, D = 2, E = 3, F = 4;
const G = 1 + 2, H = [1, 2], I = A;
const int J = 1, K = 2;

// a single constant must keep working, with and without an attribute

const L = 1;
const int M = 2;

#[\Deprecated]
const N = 3;

// as must class constants

class C1
{
    const A = 1, B = 2;
    public const int C = 1, D = 2;
    final public const E = 1;
}
