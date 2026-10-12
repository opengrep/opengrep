<?php

// a skipped element keeps the positions of the elements after it also in a
// nested destructuring and in a foreach

function test() {
    [[, $a], , $b] = [[safe(), source()], source(), safe()];
    //ruleid: test-list-skipped-element-nested
    sink($a);
    //OK: test-list-skipped-element-nested
    sink($b);

    list(list(, $c), $d) = [[source(), safe()], source()];
    //OK: test-list-skipped-element-nested
    sink($c);
    //ruleid: test-list-skipped-element-nested
    sink($d);

    foreach ([[safe(), source()]] as [, $e]) {
        //ruleid: test-list-skipped-element-nested
        sink($e);
    }
}
