<?php

// a group use may end with a comma

use Foo\Baz\{
    const X,
    function fn_b,
    Klass as Alias,
};

use function Other\{fn_c,};

fn_a();
//MATCH:
fn_b();
fn_c();
