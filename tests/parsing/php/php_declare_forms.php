<?php

// A declare directive says how the file is compiled rather than what it does,
// so every form of it is accepted and only the statement it introduces is
// kept.

declare(strict_types=1);
declare(ticks=1);
declare(encoding="UTF-8");
declare(encoding='utf-8');
declare(encoding = 1);
declare(A=1, B=2);

declare(ticks=1) {
    $a = 1;
}

declare(strict_types=1) {
    function inside()
    {
        return 1;
    }
}

declare(ticks=UNKNOWN_CONST) {
    $b = 2;
}

declare(ticks=1):
    $c = 3;
enddeclare;

function after()
{
    return 1;
}
