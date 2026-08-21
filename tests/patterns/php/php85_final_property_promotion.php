<?php

// 'final' on a promoted property must reach the generic AST, so that a rule
// can tell a final promoted property from an ordinary one

class A
{
    //MATCH:
    public function __construct(final public int $x) {}
}

class B
{
    //OK: not final
    public function __construct(public int $x) {}
}

class C
{
    //OK: final, but no visibility, so the pattern's 'public' does not apply
    public function __construct(final int $y) {}
}

class D
{
    //OK: not promoted at all
    public function __construct(int $z) {}
}
