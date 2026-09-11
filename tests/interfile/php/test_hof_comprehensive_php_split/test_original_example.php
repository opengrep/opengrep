<?php

function test_original_example() {
    $history = getHistory("name", "owner");
    customForEach([$history], function($node) {
        $changes = $node;
        // ruleid: test-hof-taint
        sink($changes);
    });
}
