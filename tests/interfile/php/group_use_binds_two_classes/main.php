<?php
namespace Main;

use App\{Store, Cache};

function go() {
    Store::run($_GET["x"]);
    Cache::put($_GET["y"]);
}
