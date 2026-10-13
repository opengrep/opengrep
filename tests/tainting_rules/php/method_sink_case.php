<?php

// a sink method matches the call however its name is cased

function run($db) {
    $id = $_GET['id'];

    //ruleid: test-method-sink-case
    $db->query($id);

    //ruleid: test-method-sink-case
    $db->Query($id);

    //ruleid: test-method-sink-case
    $db->QUERY($id);

    //OK: test-method-sink-case
    $db->quote($id);
}
