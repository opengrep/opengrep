<?php

// PHP 8.4: 'public(set)' is a modifier of its own, with or without a type

//MATCH:
class A { public(set) int $a; }

//MATCH:
class B { public public(set) int $a; }

//MATCH:
class C { public public(set) readonly int $a; }

//MATCH:
class D { public(set) readonly int $a; }

//MATCH: any spacing and case
class E { PUBLIC ( Set ) string $a; }

//OK: no type
class F { public public(set) $a; }

//OK: 'private(set)'
class G { public private(set) int $a; }

//OK: no asymmetric visibility
class H { public int $a; }
