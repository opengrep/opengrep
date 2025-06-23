<?php
// Note that we use bad to separate the apearance of $filename in match from that later on. 
// the idea is that we need to make sure the code is parsed with menir and so the 
// treesitter problem of string parsing does not propagate. See https://github.com/opengrep/opengrep/issues/297
function f2($filename) {
    // ruleid: match_test
    return match($filename) {
             // ruleid: test
              1 => bad("convert $filename"),
              2 => 'two',
              default => 'more',
};
} 
