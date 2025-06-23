<?php
// Note that we use bad to separate the apearance of $filename in f from that of f2. 
// the idea is that we need to make sure the code is parsed with menir and so the 
// treesitter problem of string parsing does not propagate. See https://github.com/opengrep/opengrep/issues/297
function f($filename) {
    // ruleid: test
    bad("convert $filename");
}

function f2($filename) {
    // ruleid: test
    return match($filename) {
              1 => 'one',
              2 => 'two',
              default => 'more',
};
} 
