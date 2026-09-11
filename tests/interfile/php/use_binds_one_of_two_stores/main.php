<?php
namespace Main;

use App\Store;

function go() {
    Store::run($_GET["x"]);
}
