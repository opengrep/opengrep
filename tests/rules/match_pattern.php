<?php

function f($filename) {
    // ruleid: test
    "convert $filename";
}
function f1($filename) {
  // ruleid: test
   match($filename) {
    1 => 'one',
    2 => 'two',
};

}
