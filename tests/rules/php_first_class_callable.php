<?php
// A first-class callable such as strlen(...) is the callable itself, not a call
// to Closure::fromCallable; only a call written in the source matches.

# ok:first-class-callable
$fn1 = strlen(...);

# ok:first-class-callable
$fn2 = $obj->method(...);

# ok:first-class-callable
$fn3 = Foo::bar(...);

# ruleid:first-class-callable
$fn4 = Closure::fromCallable('strlen');

# ok:first-class-callable
$fn5 = strlen("hello");

# ok:first-class-callable
$fn6 = $obj->method("arg");
