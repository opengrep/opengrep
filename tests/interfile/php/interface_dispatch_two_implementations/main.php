<?php
namespace App;

function go(Handler $h) {
    $h->handle($_GET["x"]);
}
