<?php

use function Amp\async;

function run(array $jobs): void
{
    //MATCH:
    $future = async(static fn () => $jobs[0]->run());

    //MATCH:
    $futures[] = async(static function () use ($jobs): void {
        $jobs[1]->run();
    });

    //MATCH: root-qualified
    \Amp\async(fn () => 1);

    //OK: a method of that name
    $loop->async(fn () => 2);

    //OK: a first-class callable, which does not call it
    $run = async(...);
}
