<?php

function f($filename) {
    // ruleid: test
    bad("convert $filename");
}
function f2($filename) {
    // ruleid: test
    return match($filename) {
              1 => 'one',
              2 => 'two'
};
}
