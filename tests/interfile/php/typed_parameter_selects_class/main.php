<?php
namespace Main;

use App\Store;

function go(Store $s) {
    $s->run($_GET["x"]);
}
