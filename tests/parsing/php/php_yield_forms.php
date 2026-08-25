<?php

// 'yield' may appear with no operand at all, which is how a generator both
// yields null and receives a value sent back into it.

function g()
{
    yield;
    yield;

    $received = yield;

    // the forms that already worked
    yield 1;
    yield $v;
    yield $k => $v;
    yield from foo();
    yield from [1, 2, 3];

    $a = (yield);
    $b = (yield 1);

    if (yield) {
    }

    return;
}

function h()
{
    yield break;
}

function nested()
{
    yield from (function () { yield; })();
}
