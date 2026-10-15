<?php

// PHP 8.4: asymmetric visibility on a promoted constructor parameter

class A
{
    //MATCH:
    public function __construct(public public(set) int $a) {}
}

class B
{
    //MATCH:
    public function __construct(
        private int $b,
        public(set) string $c,
    ) {}
}

class C
{
    //OK: 'private(set)'
    public function __construct(public private(set) int $a) {}
}

class D
{
    //OK: no asymmetric visibility
    public function __construct(public int $a) {}
}
