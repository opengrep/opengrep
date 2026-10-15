<?php

// PHP 8.4: 'protected(set)' is a modifier of its own, and a pattern without
// a type matches it with any type or none

//MATCH:
class A { public protected(set) int $a; }

//MATCH:
class B { public protected(set) $a; }

//MATCH:
class C { protected(set) readonly array $a; }

//OK: 'public(set)'
class D { public public(set) int $a; }

//OK: a plain 'protected'
class E { protected int $a; }
