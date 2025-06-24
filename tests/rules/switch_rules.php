<?php
function f($arg) {
    //ruleid: switch_test
    switch ($source) {
        case 0:
            $a = 0;
            //ruleid: test
            bad(" here $arg");
        default:
            $d = 0;
    }
}
