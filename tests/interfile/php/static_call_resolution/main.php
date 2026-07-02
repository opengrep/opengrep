<?php

require_once 'lib.php';
require_once 'model.php';

function handle($dbh) {
    // SOURCE: user-controlled HTTP parameter.
    $q = $_GET['q'];
    // Static method call: must resolve to Db::query (lib.php),
    // not to Model::query (model.php).
    return Db::query($dbh, $q);
}
