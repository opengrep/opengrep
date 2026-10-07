<?php

// PHP 8.2: 'readonly' combines with 'final' or 'abstract', in either order

//ERROR:
readonly class A {}

//ERROR:
final readonly class B {}

//ERROR: the other order
readonly final class C {}

//ERROR:
abstract readonly class D {}

// OK: not readonly
final class E {}

// OK: no modifier at all
class F {}

// OK: an anonymous class, which PHP 8.3 lets be readonly
$anon = new readonly class {};
