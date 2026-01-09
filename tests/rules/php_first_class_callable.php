<?php
// Test that Closure::fromCallable pattern matches PHP 8.1 first-class callable syntax
// strlen(...) is desugared to Closure::fromCallable('strlen')

# ruleid:first-class-callable
$fn1 = strlen(...);

# ruleid:first-class-callable
$fn2 = $obj->method(...);

# ruleid:first-class-callable
$fn3 = Foo::bar(...);

# ruleid:first-class-callable
$fn4 = Closure::fromCallable('strlen');

# ok:first-class-callable
$fn5 = strlen("hello");

# ok:first-class-callable
$fn6 = $obj->method("arg");
