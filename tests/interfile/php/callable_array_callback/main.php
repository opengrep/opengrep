<?php
namespace Main;

use App\Store;

function go() {
    $obj = new Store();
    array_map([$obj, 'handle'], [$_GET["x"]]);
}
