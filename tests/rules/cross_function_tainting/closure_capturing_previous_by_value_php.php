<?php
function build($items) {
    $f = source();
    foreach ($items as $i) {
        $f = function () use ($f) { return $f; };
    }
    // ruleid: closure_capturing_previous_by_value_php
    sink($f);
}
