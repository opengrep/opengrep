<?php

// PHP 8.4: 'private(set)' is a modifier of its own

//MATCH:
class A { public private(set) int $a; }

//MATCH:
class B { private(set) string $a; }

//MATCH: on a property with hooks
class C {
    public private(set) int $a {
        set => $value * 2;
    }
}

//MATCH: any spacing and case
class D { public PRIVATE (set) int $a; }

//OK: 'protected(set)'
class E { public protected(set) int $a; }

//OK: a plain 'private'
class F { private int $a; }
