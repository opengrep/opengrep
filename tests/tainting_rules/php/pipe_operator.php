<?php

// The PHP 8.5 pipe operator is desugared into the call it stands for, so
// taint has to flow through it exactly as through a direct call.
// coupling: AST_to_IL.ml, the OtherExpr("PipelineCall", ...) case

function test() {
    $a = source();
    //ruleid: test-pipe-operator
    sink($a);

    $b = source();
    //ruleid: test-pipe-operator
    $b |> sink(...);

    $c = source();
    //ruleid: test-pipe-operator
    $c |> escape(...) |> sink(...);

    $d = source();
    //ruleid: test-pipe-operator
    $d |> sink(...) |> report(...);

    $e = safe();
    //OK:
    $e |> sink(...);
}
