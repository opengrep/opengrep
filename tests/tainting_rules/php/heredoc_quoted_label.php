<?php

// a heredoc with a double-quoted label interpolates its body, so taint flows
// from the variables in it

function test() {
    $a = source();

    //ruleid: test-heredoc-quoted-label
    sink(<<<"EOT"
        value: $a
        EOT);

    //ruleid: test-heredoc-quoted-label
    sink(<<<"EOT"
    value: {$a}
    EOT);

    $b = safe();
    //OK: test-heredoc-quoted-label
    sink(<<<"EOT"
        value: $b
        EOT);
}
