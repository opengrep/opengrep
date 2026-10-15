<?php

function test() {
    [, $a] = [safe(), source()];
    //ruleid: test-list-skipped-element
    sink($a);

    [, $b] = [source(), safe()];
    //OK:
    sink($b);

    [$c, , $d] = [safe(), source(), safe()];
    //OK:
    sink($d);

    list(, $e) = [safe(), source()];
    //ruleid: test-list-skipped-element
    sink($e);

    list(, $f) = [source(), safe()];
    //OK:
    sink($f);

    [$g, $h,] = [safe(), source()];
    //ruleid: test-list-skipped-element
    sink($h);

    list($i, $j,) = [safe(), source()];
    //ruleid: test-list-skipped-element
    sink($j);
}
