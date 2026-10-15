<?php

// a cast sanitizes whatever its name's case

function load($db) {
    //OK: test-cast-case-insensitive
    $db->query("SELECT * FROM t WHERE id = " . (int) $_GET['id']);

    //OK: test-cast-case-insensitive
    $db->query("SELECT * FROM t WHERE id = " . (INT) $_GET['id']);

    //OK: test-cast-case-insensitive
    $db->query("SELECT * FROM t WHERE id = " . (Integer) $_GET['id']);

    //ruleid: test-cast-case-insensitive
    $db->query("SELECT * FROM t WHERE id = " . (String) $_GET['id']);
}
